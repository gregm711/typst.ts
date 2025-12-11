pub mod builder;

#[cfg(feature = "incr")]
mod incr;
pub(crate) mod utils;

#[cfg(all(test, feature = "incr"))]
mod debug_line_boxes;

#[cfg(all(test, feature = "incr"))]
mod test_ieee_template;

#[cfg(all(test, feature = "incr"))]
mod test_glyph_maps;

#[cfg(all(test, feature = "incr"))]
mod test_span_comparison;

pub use crate::builder::TypstFontResolver;
pub use reflexo_typst::*;

use core::fmt;
use std::collections::HashSet;
use std::{fmt::Write, path::Path, sync::Arc};

use error::TypstSourceDiagnostic;
use font::cache::FontInfoCache;
use js_sys::{Array, JsString, Uint8Array};
use reflexo_typst::error::{long_diag_from_std, DiagMessage};
use reflexo_typst::package::registry::JsRegistry;
use reflexo_typst::prelude::EcoVec;
use reflexo_typst::typst::diag::{SourceResult, Warned};
use reflexo_typst::typst::foundations::IntoValue;
use reflexo_typst::vfs::browser::ProxyAccessModel;
use wasm_bindgen::prelude::*;

use crate::font::FontResolverImpl;
use crate::utils::console_log;
#[cfg(feature = "incr")]
use incr::IncrServer;
#[cfg(feature = "incr")]
use tinymist_world::OptionDocumentTask;
#[cfg(feature = "incr")]
use tinymist_world::CompilerFeat;
#[cfg(feature = "incr")]
use ::typst::ecow::EcoString;
#[cfg(feature = "incr")]
use ::typst::text::{Font, FontBook};
#[cfg(feature = "incr")]
use ::typst::foundations::{Bytes, Datetime};
#[cfg(feature = "incr")]
use ::typst::diag::FileResult;
#[cfg(feature = "incr")]
use ::typst::syntax::{FileId, Source};
#[cfg(feature = "incr")]
use ::typst::syntax::package::PackageSpec;
#[cfg(feature = "incr")]
use ::typst::utils::LazyHash;
#[cfg(feature = "incr")]
use ::typst::{Library, World as TypstWorld};

#[cfg(feature = "incr")]
struct IdeWorldWrapper<'a, F: CompilerFeat> {
    world: &'a reflexo_typst::world::CompilerWorld<F>,
}

#[cfg(feature = "incr")]
impl<'a, F: CompilerFeat> TypstWorld for IdeWorldWrapper<'a, F> {
    fn library(&self) -> &LazyHash<Library> {
        self.world.library()
    }

    fn book(&self) -> &LazyHash<FontBook> {
        self.world.book()
    }

    fn main(&self) -> FileId {
        self.world.main()
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        self.world.source(id)
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        self.world.file(id)
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.world.font(index)
    }

    fn today(&self, offset: Option<i64>) -> Option<Datetime> {
        self.world.today(offset)
    }
}

#[cfg(feature = "incr")]
impl<'a, F: CompilerFeat> typst_ide::IdeWorld for IdeWorldWrapper<'a, F> {
    fn upcast(&self) -> &dyn TypstWorld {
        self
    }

    fn packages(&self) -> &[(PackageSpec, Option<EcoString>)] {
        &[]
    }

    fn files(&self) -> Vec<FileId> {
        Vec::new()
    }
}

/// Line box for consistent cursor height.
/// Represents a visual line inferred from text item Y positions.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
pub(crate) struct LineBox {
    /// Page number (0-indexed)
    page: u32,
    /// Top Y position in points
    top: f32,
    /// Line height in points
    height: f32,
    /// Baseline Y position in points
    baseline: f32,
    /// Start byte offset in source
    start_byte: usize,
    /// End byte offset in source
    end_byte: usize,
}

/// Glyph map for a single text span.
/// Maps each visual glyph index to its byte offset in source.
/// This enables accurate click-to-cursor positioning without heuristics.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
pub(crate) struct SpanGlyphMap {
    /// Span ID (hex string matching data-span in SVG)
    #[serde(rename = "spanId")]
    span_id: String,
    /// Byte offset for each glyph. Index i = glyph i's start byte.
    /// Length is glyphs.len() + 1 to include end position.
    #[serde(rename = "byteOffsets")]
    byte_offsets: Vec<usize>,
}

/// Glyph maps grouped by page for efficient transfer.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
pub(crate) struct PageGlyphMap {
    /// Page number (0-indexed)
    page: u32,
    /// Version for cache invalidation
    version: u32,
    /// Glyph maps for all text spans on this page
    spans: Vec<SpanGlyphMap>,
}

/// Internal helper for geometric line verification.
#[cfg(feature = "incr")]
#[derive(Clone)]
struct TextItemInfo {
    baseline: f64,
    top: f64,
    bottom: f64,
    start: usize,
    end: usize,
}

/// Syntax node information for editor protection.
/// Identifies structural elements (headings, lists, math, etc.) with their marker boundaries.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
struct SyntaxNodeInfo {
    /// Syntax node kind: "Heading", "ListItem", "Equation", etc.
    kind: String,
    /// Span ID (hex string) for correlating with rendered spans
    #[serde(rename = "spanId")]
    span_id: String,
    /// Full node byte range start
    start: usize,
    /// Full node byte range end
    end: usize,
    /// Opening marker byte range start (e.g., "=" for headings, "-" for lists, first "$" for math)
    #[serde(rename = "markerStart", skip_serializing_if = "Option::is_none")]
    marker_start: Option<usize>,
    /// Opening marker byte range end
    #[serde(rename = "markerEnd", skip_serializing_if = "Option::is_none")]
    marker_end: Option<usize>,
    /// Content byte range start (user content after opening marker)
    #[serde(rename = "contentStart", skip_serializing_if = "Option::is_none")]
    content_start: Option<usize>,
    /// Content byte range end (before closing marker, or node end if no closing marker)
    #[serde(rename = "contentEnd", skip_serializing_if = "Option::is_none")]
    content_end: Option<usize>,
    /// Closing marker byte range start (e.g., second "$" for math, second "*" for strong)
    #[serde(rename = "closingMarkerStart", skip_serializing_if = "Option::is_none")]
    closing_marker_start: Option<usize>,
    /// Closing marker byte range end
    #[serde(rename = "closingMarkerEnd", skip_serializing_if = "Option::is_none")]
    closing_marker_end: Option<usize>,
    /// Heading depth (number of = signs)
    #[serde(skip_serializing_if = "Option::is_none")]
    depth: Option<usize>,
}

/// Tooltip payload for LSP-lite responses.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
struct TooltipPayload {
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<String>,
    title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    detail: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    range: Option<(u32, u32)>,
}

/// Definition payload for LSP-lite responses.
#[cfg(feature = "incr")]
#[derive(serde::Serialize)]
struct DefinitionPayload {
    #[serde(skip_serializing_if = "Option::is_none")]
    path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", rename = "byteRange")]
    byte_range: Option<(u32, u32)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    snippet: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
}

/// Result of marker boundary detection (internal helper)
#[cfg(feature = "incr")]
struct MarkerBoundaries {
    marker_start: Option<usize>,
    marker_end: Option<usize>,
    content_start: Option<usize>,
    content_end: Option<usize>,
    closing_marker_start: Option<usize>,
    closing_marker_end: Option<usize>,
    depth: Option<usize>,
}

/// Accumulated transform state for line collection.
/// Uses full 2D affine transform matrix for accurate positioning.
/// Matrix format: [sx kx tx; ky sy ty; 0 0 1]
#[cfg(feature = "incr")]
#[derive(Clone, Copy)]
struct TransformState {
    /// Scale X
    sx: f64,
    /// Skew Y
    ky: f64,
    /// Skew X
    kx: f64,
    /// Scale Y
    sy: f64,
    /// Translate X
    tx: f64,
    /// Translate Y
    ty: f64,
}

#[cfg(feature = "incr")]
impl TransformState {
    fn identity() -> Self {
        Self {
            sx: 1.0,
            ky: 0.0,
            kx: 0.0,
            sy: 1.0,
            tx: 0.0,
            ty: 0.0,
        }
    }

    /// Pre-concat a translation: self = self * translate(x, y)
    /// This is what typst2vec does with pre_translate.
    fn pre_translate(&self, x: f64, y: f64) -> Self {
        // Matrix multiplication: self * translate(x, y)
        // translate(x, y) = [1 0 x; 0 1 y; 0 0 1]
        // Result tx' = sx * x + kx * y + tx
        // Result ty' = ky * x + sy * y + ty
        Self {
            sx: self.sx,
            ky: self.ky,
            kx: self.kx,
            sy: self.sy,
            tx: self.sx * x + self.kx * y + self.tx,
            ty: self.ky * x + self.sy * y + self.ty,
        }
    }

    /// Pre-concat a transform: self = self * other
    /// This is what typst2vec does with pre_concat.
    fn pre_concat(&self, other: &::typst::layout::Transform) -> Self {
        let o_sx = other.sx.get();
        let o_ky = other.ky.get();
        let o_kx = other.kx.get();
        let o_sy = other.sy.get();
        let o_tx = other.tx.to_pt();
        let o_ty = other.ty.to_pt();

        // Matrix multiplication: self * other
        Self {
            sx: self.sx * o_sx + self.kx * o_ky,
            ky: self.ky * o_sx + self.sy * o_ky,
            kx: self.sx * o_kx + self.kx * o_sy,
            sy: self.ky * o_kx + self.sy * o_sy,
            tx: self.sx * o_tx + self.kx * o_ty + self.tx,
            ty: self.ky * o_tx + self.sy * o_ty + self.ty,
        }
    }

    /// Get the accumulated Y position (translation)
    fn pos_y(&self) -> f64 {
        self.ty
    }

    /// Get the accumulated Y scale (for height scaling)
    fn scale_y(&self) -> f64 {
        self.sy
    }

    /// Apply the transform to a point and return the resulting coordinates.
    fn apply_to_point(&self, x: f64, y: f64) -> (f64, f64) {
        let new_x = self.sx * x + self.kx * y + self.tx;
        let new_y = self.ky * x + self.sy * y + self.ty;
        (new_x, new_y)
    }
}

#[cfg(feature = "incr")]
fn console_log(_msg: impl AsRef<str>) {
    #[cfg(target_arch = "wasm32")]
    {
        web_sys::console::log_1(&_msg.as_ref().into());
    }
}

/// In format of
///
/// ```log
/// // with package
/// cetz:0.2.0@lib.typ:2:9-3:15: error: unexpected type in `+` application
/// // without package
/// main.typ:2:9-3:15: error: unexpected type in `+` application
/// ```
struct UnixFmt(DiagMessage);

impl fmt::Display for UnixFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.package.is_empty() {
        } else {
            f.write_str(&self.0.package)?;
            f.write_char('@')?;
        }
        f.write_str(&self.0.path)?;
        f.write_char(':')?;

        if let Some(r) = self.0.range.as_ref() {
            write!(f, "{}:{}:", r.start.line + 1, r.start.character + 1)?;
        }

        write!(f, " {}: {}", self.0.severity, self.0.message)
    }
}

fn convert_diag<'a>(
    e: impl Iterator<Item = &'a TypstSourceDiagnostic>,
    world: Option<&dyn TypstWorld>,
    has_error: bool,
    diagnostics_format: u8,
) -> JsValue {
    fn convert_diag_object(e: DiagMessage) -> JsValue {
        let obj = js_sys::Object::new();
        js_sys::Reflect::set(&obj, &"package".into(), &e.package.into()).unwrap();
        js_sys::Reflect::set(&obj, &"path".into(), &e.path.into()).unwrap();
        if let Some(range) = e.range {
            let rng = format!(
                "{}:{}-{}:{}",
                range.start.line, range.start.character, range.end.line, range.end.character
            )
            .into();
            js_sys::Reflect::set(&obj, &"range".into(), &rng).unwrap();
        } else {
            js_sys::Reflect::set(&obj, &"range".into(), &"".into()).unwrap();
        }
        js_sys::Reflect::set(&obj, &"severity".into(), &e.severity.to_string().into()).unwrap();
        js_sys::Reflect::set(&obj, &"message".into(), &e.message.as_str().into()).unwrap();
        obj.into()
    }

    let res = e
        .into_iter()
        .flat_map(move |e| long_diag_from_std(e, world))
        .map(|e| {
            if diagnostics_format == 3 {
                convert_diag_object(e)
            } else {
                format!("{}", UnixFmt(e)).into()
            }
        });

    let diag = Array::from_iter(res).into();

    let res = js_sys::Object::new();
    js_sys::Reflect::set(&res, &"hasError".into(), &has_error.into()).unwrap();
    js_sys::Reflect::set(&res, &"diagnostics".into(), &diag).unwrap();
    res.into()
}

#[wasm_bindgen]
pub struct TypstCompiler {
    pub(crate) verse: TypstBrowserUniverse,
}

impl TypstCompiler {
    pub fn new(
        access_model: ProxyAccessModel,
        registry: JsRegistry,
        fonts: FontResolverImpl,
    ) -> Result<Self, JsValue> {
        Ok(Self {
            verse: TypstBrowserUniverse::new(
                std::path::Path::new("/").to_owned(),
                None,
                access_model,
                registry,
                fonts,
            ),
        })
    }
}

/// @deprecated use TypstFontResolverBuilder instead
#[wasm_bindgen]
pub fn get_font_info(buffer: Uint8Array) -> JsValue {
    serde_wasm_bindgen::to_value(&FontInfoCache::from_data(buffer.to_vec().as_slice())).unwrap()
}

// todo: design error handling
// todo: we return a string for now which is better than nothing
#[wasm_bindgen]
#[allow(non_snake_case)]
impl TypstCompiler {
    pub fn reset(&mut self) -> Result<(), JsValue> {
        // reset the world caches
        self.verse.evict(30);

        Ok(())
    }

    pub fn set_fonts(&mut self, fonts: &TypstFontResolver) -> Result<(), JsValue> {
        self.verse
            .increment_revision(|verse| verse.set_fonts(fonts.fonts.clone()));
        Ok(())
    }

    pub fn set_inputs(&mut self, inputs: JsValue) -> Result<(), JsValue> {
        let inputs: std::collections::HashMap<String, String> =
            serde_wasm_bindgen::from_value(inputs).map_err(|e| format!("{e:?}"))?;
        let inputs = inputs
            .into_iter()
            .map(|(k, v)| (k.into(), v.into_value()))
            .collect();
        self.verse
            .increment_revision(|verse| verse.set_inputs(Arc::new(LazyHash::new(inputs))));
        Ok(())
    }

    pub fn add_source(&mut self, path: &str, content: &str) -> bool {
        let path = Path::new(path).to_owned();
        match self
            .verse
            .map_shadow(&path, Bytes::from_string(content.to_owned()))
        {
            Ok(_) => true,
            Err(e) => {
                console_log!("Error: {:?}", e);
                false
            }
        }
    }

    pub fn map_shadow(&mut self, path: &str, content: &[u8]) -> bool {
        let path = Path::new(path).to_owned();
        match self.verse.map_shadow(&path, Bytes::new(content.to_owned())) {
            Ok(_) => true,
            Err(e) => {
                console_log!("Error: {:?}", e);
                false
            }
        }
    }

    pub fn unmap_shadow(&mut self, path: &str) -> bool {
        let path = Path::new(path).to_owned();
        match self.verse.unmap_shadow(&path) {
            Ok(_) => true,
            Err(e) => {
                console_log!("Error: {:?}", e);
                false
            }
        }
    }

    pub fn reset_shadow(&mut self) {
        self.verse.reset_shadow()
    }

    // todo: font manipulation
    // pub fn modify_font_data(&mut self, idx: usize, buffer: Uint8Array) {}
    // pub fn rebuild(&mut self) {}

    pub fn get_loaded_fonts(&mut self) -> Vec<JsString> {
        self.verse
            .font_resolver
            .loaded_fonts()
            .map(|s| format!("<{}, {:?}>", s.0, s.1).into())
            .collect()
    }

    #[cfg(feature = "ast")]
    pub fn get_ast(&mut self, main_file_path: String) -> Result<String, JsValue> {
        self.verse
            .increment_revision(|verse| verse.set_entry_file(Path::new(&main_file_path).into()))
            .map_err(|e| format!("{e:?}"))?;
        let world = self.verse.snapshot();

        // export ast
        let src = world.main();
        let src = world.source(src).unwrap();

        let mut cursor = std::io::Cursor::new(Vec::new());
        reflexo_typst::dump_ast(
            &src.id().vpath().as_rootless_path().display().to_string(),
            &src,
            &mut cursor,
        )
        .map_err(|e| format!("{e:?}"))?;
        let data = cursor.into_inner();

        let converted = ansi_to_html::convert(
            String::from_utf8(data)
                .map_err(|e| format!("{e:?}"))?
                .as_str(),
        )
        .map_err(|e| format!("{e:?}"))?;
        Ok(converted)
    }

    pub fn get_semantic_token_legend(&mut self) -> Result<JsValue, JsValue> {
        let tokens = self.verse.get_semantic_token_legend();
        serde_wasm_bindgen::to_value(tokens.as_ref()).map_err(|e| format!("{e:?}").into())
    }

    #[cfg(feature = "semantic_tokens")]
    pub fn get_semantic_tokens(
        &mut self,
        offset_encoding: String,
        file_path: Option<String>,
        result_id: Option<String>,
    ) -> Result<js_sys::Object, JsValue> {
        use js_sys::Uint32Array;
        use reflexo_typst::parser::OffsetEncoding;
        if let Some(result_id) = result_id {
            return Err(
                error_once!("Not implemented", result_id: format!("{:?}", result_id)).into(),
            );
        }

        let tokens = self.verse.get_semantic_tokens(
            file_path,
            match offset_encoding.as_str() {
               "utf-16" => OffsetEncoding::Utf16,
              "utf-8" => OffsetEncoding::Utf8,
                _ => {
                    return Err(error_once!("Unsupported offset encoding", offset_encoding: offset_encoding).into());
                }
            },
        )?;
        let mut result = Vec::new();
        for token in tokens.iter() {
            result.push(token.delta_line);
            result.push(token.delta_start_character);
            result.push(token.length);
            result.push(token.token_type);
            result.push(token.token_modifiers);
        }

        let semantic_tokens = js_sys::Object::new();
        js_sys::Reflect::set(
            &semantic_tokens,
            &"data".into(),
            &Uint32Array::from(&result[..]).into(),
        )?;
        js_sys::Reflect::set(
            &semantic_tokens,
            &"resultId".into(),
            &JsString::from("").into(),
        )?;

        Ok(semantic_tokens)
    }

    pub fn snapshot(
        &mut self,
        root: Option<String>,
        main_file_path: Option<String>,
        inputs: Option<Vec<js_sys::Array>>,
    ) -> Result<TypstCompileWorld, JsValue> {
        let inputs = inputs.map(|inputs| Arc::new(LazyHash::new(convert_inputs(&inputs))));

        let entry = if let Some(root) = root {
            EntryState::new_workspace(Path::new(&root).into())
        } else {
            self.verse.entry_state()
        };

        let entry = if let Some(main_file_path) = main_file_path {
            entry
                .try_select_path_in_workspace(Path::new(&main_file_path))?
                .ok_or_else(|| error_once!("failed to select path", path: main_file_path))?
        } else {
            entry.clone()
        };

        let world = self.verse.snapshot_with(Some(TaskInputs {
            entry: Some(entry),
            inputs,
        }));

        Ok(TypstCompileWorld {
            graph: WorldComputeGraph::new(CompileSnapshot::from_world(world)),
        })
    }

    pub fn get_artifact(
        &mut self,
        fmt: String,
        diagnostics_format: u8,
    ) -> Result<JsValue, JsValue> {
        self.compile(None, None, fmt, diagnostics_format)
    }

    pub fn compile(
        &mut self,
        main_file_path: Option<String>,
        inputs: Option<Vec<js_sys::Array>>,
        fmt: String,
        diagnostics_format: u8,
    ) -> Result<JsValue, JsValue> {
        let mut w = self.snapshot(None, main_file_path, inputs)?;
        let fmt = match fmt.as_str() {
            "vector" => 0u8,
            "pdf" => 1,
            "_dummy" => 2,
            _ => return Err(error_once!("Unsupported fmt", fmt: fmt).into()),
        };
        w.get_artifact(fmt, diagnostics_format)
    }

    pub fn query(
        &mut self,
        main_file_path: String,
        inputs: Option<Vec<js_sys::Array>>,
        selector: String,
        field: Option<String>,
    ) -> Result<String, JsValue> {
        let mut w = self.snapshot(None, Some(main_file_path), inputs)?;
        let _doc = w.compile(0, 0)?;
        w.query(0, selector, field)
    }

    #[cfg(feature = "incr")]
    pub fn create_incr_server(&mut self) -> Result<IncrServer, JsValue> {
        Ok(IncrServer::default())
    }

    #[cfg(feature = "incr")]
    pub fn incr_compile(
        &mut self,
        main_file_path: String,
        inputs: Option<Vec<js_sys::Array>>,
        state: &mut IncrServer,
        diagnostics_format: u8,
    ) -> Result<JsValue, JsValue> {
        let mut w = self.snapshot(None, Some(main_file_path), inputs)?;
        w.incr_compile(state, diagnostics_format)
    }

    /// Extract glyph data (maps + positions) for specific pages from the cached document.
    /// This is a pure extraction - no incremental delta, no state changes.
    /// Used for hydration when scrolling to pages not in the initial viewport.
    ///
    /// Takes:
    /// - main_file_path: Path to the main Typst file (for source resolution)
    /// - state: IncrServer with cached document
    /// - pages: Array of page indices (0-indexed)
    ///
    /// Returns a JS object with:
    /// - glyphMaps: array of {page, version, spans: [{spanId, byteOffsets}]}
    /// - glyphPositions: array of {page, spans: [{span, positions, glyph_spans, ...}]}
    /// - layoutMap: array of {page, spans: [{span, x, y, width, height}]}
    ///
    /// Returns null if no document is cached or if pages array is empty.
    #[cfg(feature = "incr")]
    pub fn extract_glyph_data(
        &mut self,
        main_file_path: String,
        state: &IncrServer,
        pages: &JsValue,
    ) -> Result<JsValue, JsValue> {
        let w = self.snapshot(None, Some(main_file_path), None)?;
        w.extract_glyph_data(state, pages)
    }

    #[cfg(feature = "incr")]
    pub fn get_tooltip(&mut self, main_file_path: String, byte_offset: u32) -> Result<JsValue, JsValue> {
        let w = self.snapshot(None, Some(main_file_path), None)?;
        w.get_tooltip(byte_offset)
    }

    #[cfg(feature = "incr")]
    pub fn get_definition(&mut self, main_file_path: String, byte_offset: u32) -> Result<JsValue, JsValue> {
        let w = self.snapshot(None, Some(main_file_path), None)?;
        w.get_definition(byte_offset)
    }

    #[cfg(feature = "incr")]
    pub fn get_autocomplete(&mut self, main_file_path: String, byte_offset: u32) -> Result<JsValue, JsValue> {
        let w = self.snapshot(None, Some(main_file_path), None)?;
        w.get_autocomplete(byte_offset)
    }
}

type CFlag<D> = FlagTask<CompilationTask<D>>;
type PagedCFlag = CFlag<reflexo_typst::TypstPagedDocument>;
type HtmlCFlag = CFlag<reflexo_typst::TypstHtmlDocument>;

#[wasm_bindgen]
pub struct TypstCompileWorld {
    graph: Arc<WorldComputeGraph<BrowserCompilerFeat>>,
}

#[wasm_bindgen]
impl TypstCompileWorld {
    #[cfg(feature = "incr")]
    pub fn get_autocomplete(&self, byte_offset: u32) -> Result<JsValue, JsValue> {
        use reflexo_typst::typst::syntax::Source;
        use typst_ide::autocomplete;

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source: Source = world
            .source(main_id)
            .map_err(|e| JsValue::from_str(&format!("{e:?}")))?;

        let doc = self
            .graph
            .compute::<OptionDocumentTask<TypstPagedDocument>>()
            .map_err(|e| JsValue::from_str(&format!("{e:?}")))?;
        let doc = match doc.as_ref() {
            Some(doc) => doc.clone(),
            None => return Ok(JsValue::NULL),
        };

        let cursor = byte_offset as usize;

        let world_adapter = IdeWorldWrapper { world };

        let result = autocomplete(&world_adapter, Some(doc.as_ref()), &source, cursor, true);

        if let Some((from, items)) = result {
            #[derive(serde::Serialize)]
            struct CompletionPayload {
                label: String,
                #[serde(skip_serializing_if = "Option::is_none")]
                apply: Option<String>,
                #[serde(skip_serializing_if = "Option::is_none")]
                detail: Option<String>,
                #[serde(skip_serializing_if = "Option::is_none")]
                kind: Option<String>,
                range: (u32, u32),
            }

            let mapped: Vec<CompletionPayload> = items
                .into_iter()
                .map(|item| CompletionPayload {
                    label: item.label.to_string(),
                    apply: item.apply.map(|a| a.to_string()),
                    detail: item.detail.map(|d| d.to_string()),
                    kind: Some(format!("{:?}", item.kind)),
                    range: (from as u32, cursor as u32),
                })
                .collect();

            return Ok(serde_wasm_bindgen::to_value(&mapped)?);
        }

        Ok(JsValue::NULL)
    }

    #[cfg(feature = "incr")]
    pub fn get_tooltip(&self, byte_offset: u32) -> Result<JsValue, JsValue> {
        use ::typst::syntax::{Source, SyntaxKind, SyntaxNode};

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source: Source = world
            .source(main_id)
            .map_err(|e| JsValue::from_str(&format!("{e:?}")))?;
        let text = source.text();
        if (byte_offset as usize) > text.len() {
            return Ok(JsValue::NULL);
        }

        let mut best: Option<(usize, usize, SyntaxKind)> = None;

        fn visit(
            node: SyntaxNode,
            source: &Source,
            offset: usize,
            best: &mut Option<(usize, usize, SyntaxKind)>,
        ) {
            if let Some(range) = source.range(node.span()) {
                if offset >= range.start && offset < range.end {
                    let len = range.end - range.start;
                    match best {
                        Some((_, best_len, _)) if len >= *best_len => {}
                        _ => {
                            *best = Some((range.start, len, node.kind()));
                        }
                    }
                    for child in node.children() {
                        visit(child.clone(), source, offset, best);
                    }
                }
            }
        }

        visit(source.root().clone(), &source, byte_offset as usize, &mut best);

        if let Some((start, len, kind)) = best {
            let end = start + len;
            let snippet = text
                .get(start..end)
                .map(|s| {
                    let trimmed = s.trim();
                    let mut owned = trimmed.to_string();
                    if owned.len() > 200 {
                        owned.truncate(200);
                    }
                    owned
                })
                .unwrap_or_default();
            let payload = crate::TooltipPayload {
                kind: Some(format!("{:?}", kind)),
                title: format!("{:?}", kind),
                detail: if snippet.is_empty() { None } else { Some(snippet) },
                range: Some((start as u32, end as u32)),
            };
            return Ok(serde_wasm_bindgen::to_value(&payload)?);
        }

        Ok(JsValue::NULL)
    }

    #[cfg(feature = "incr")]
    pub fn get_definition(&self, byte_offset: u32) -> Result<JsValue, JsValue> {
        use ::typst::syntax::Source;

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source: Source = world
            .source(main_id)
            .map_err(|e| JsValue::from_str(&format!("{e:?}")))?;
        let text = source.text();
        if (byte_offset as usize) > text.len() {
            return Ok(JsValue::NULL);
        }

        let mut best: Option<(usize, usize)> = None;

        fn visit(
            node: ::typst::syntax::SyntaxNode,
            source: &Source,
            offset: usize,
            best: &mut Option<(usize, usize)>,
        ) {
            if let Some(range) = source.range(node.span()) {
                if offset >= range.start && offset < range.end {
                    let len = range.end - range.start;
                    match best {
                        Some((_, best_len)) if len >= *best_len => {}
                        _ => {
                            *best = Some((range.start, len));
                        }
                    }
                    for child in node.children() {
                        visit(child.clone(), source, offset, best);
                    }
                }
            }
        }

        visit(source.root().clone(), &source, byte_offset as usize, &mut best);

        if let Some((start, len)) = best {
            let end = start + len;
            let snippet = text
                .get(start..end)
                .map(|s| {
                    let trimmed = s.trim();
                    let mut owned = trimmed.to_string();
                    if owned.len() > 200 {
                        owned.truncate(200);
                    }
                    owned
                })
                .unwrap_or_default();
            let payload = crate::DefinitionPayload {
                path: None,
                byte_range: Some((start as u32, end as u32)),
                snippet: if snippet.is_empty() { None } else { Some(snippet) },
                message: None,
            };
            return Ok(serde_wasm_bindgen::to_value(&payload)?);
        }

        Ok(JsValue::NULL)
    }

    pub fn compile(&mut self, kind: u8, diagnostics_format: u8) -> Result<JsValue, JsValue> {
        match kind {
            0 => {
                self.do_compile_paged()?;
                self.get_diag::<TypstPagedDocument>(diagnostics_format)
            }
            1 => {
                self.do_compile_html()?;
                self.get_diag::<TypstHtmlDocument>(diagnostics_format)
            }
            _ => Err(error_once!("invalid kind", kind: kind).into()),
        }
    }

    pub fn title(&self, kind: u8) -> Result<Option<String>, JsValue> {
        Ok(self
            .get_doc(kind)?
            .and_then(|doc| Some(doc.info().title.as_ref()?.to_string())))
    }

    pub fn get_artifact(&mut self, fmt: u8, diagnostics_format: u8) -> Result<JsValue, JsValue> {
        #[cfg(feature = "svg")]
        use reflexo_vec2svg::DefaultExportFeature;
        #[cfg(feature = "svg")]
        type SvgModuleExport = WebSvgModuleExport<DefaultExportFeature>;
        #[cfg(feature = "pdf")]
        use reflexo_typst::task::ExportPdfTask;

        let Some(doc) = self.do_compile_paged()? else {
            return self.get_diag::<TypstPagedDocument>(diagnostics_format);
        };
        let artifact_bytes: Bytes = match fmt {
            #[cfg(feature = "svg")]
            0 => SvgModuleExport::run(&self.graph, &doc, &ExportWebSvgModuleTask::default())?,
            #[cfg(feature = "pdf")]
            1 => PdfExport::run(&self.graph, &doc, &ExportPdfTask::default())?,
            2 => Bytes::new([]),
            _ => {
                let _ = doc;
                return Err(error_once!("Unsupported fmt", format: fmt).into());
            }
        };

        let v: JsValue = Uint8Array::from(artifact_bytes.as_slice()).into();

        Ok(if diagnostics_format != 0 {
            let result = js_sys::Object::new();
            js_sys::Reflect::set(&result, &"result".into(), &v)?;
            result.into()
        } else {
            v
        })
    }

    pub fn query(
        &mut self,
        kind: u8,
        selector: String,
        field: Option<String>,
    ) -> Result<String, JsValue> {
        // todo: diagnostics
        let doc = self
            .get_doc(kind)?
            .ok_or_else(|| error_once!("document is not compiled"))?;

        // todo: query snapshot.query should directly return a error?
        let elements: Vec<typst::foundations::Content> =
            reflexo_typst::query::retrieve(&self.graph.snap.world, &selector, &doc)
                .map_err(|e| JsValue::from(e.as_str()))?;

        let mapped: Vec<_> = elements
            .into_iter()
            .filter_map(|c| match &field {
                Some(field) => c.get_by_name(field).ok(),
                _ => Some(c.into_value()),
            })
            .collect();

        Ok(serde_json::to_string_pretty(&mapped).map_err(|e| format!("{e:?}"))?)
    }

    #[cfg(feature = "incr")]
    pub fn incr_compile(
        &mut self,
        state: &mut IncrServer,
        diagnostics_format: u8,
    ) -> Result<JsValue, JsValue> {
        // Take the page filter for this compile (resets to None for next compile)
        let page_filter = state.take_glyph_map_pages();

        let Some(doc) = self.do_compile_paged()? else {
            return self.get_diag::<TypstPagedDocument>(diagnostics_format);
        };

        // Collect glyph maps for accurate click-to-cursor positioning
        // Filter to only requested pages if page_filter is set
        let glyph_maps = self.collect_glyph_maps(&doc, page_filter.as_ref());

        // Get total page count BEFORE moving doc into state.update()
        let total_page_count = doc.pages.len() as u32;

        let delta = state.update(doc);
        let v = Uint8Array::from(delta.bytes.as_slice()).into();

        // Resolve span ranges from the world for source mapping
        let span_ranges = self.resolve_span_ranges(&delta.bytes);
        let span_ranges_js = serde_wasm_bindgen::to_value(&span_ranges).unwrap_or(JsValue::NULL);

        // Filter glyph positions by page if filter is active
        let glyph_positions = if let Some(filter) = &page_filter {
            delta.glyph_map.iter()
                .filter(|gp| filter.contains(&gp.page))
                .cloned()
                .collect::<Vec<_>>()
        } else {
            delta.glyph_map.clone()
        };

        // Keep glyph positions/layout coherent with glyph maps: only emit spans that
        // have byte offsets. This prevents orphan spans (e.g., generated content) from
        // reaching the client.
        let span_ids: std::collections::HashSet<String> = glyph_maps
            .iter()
            .flat_map(|page| page.spans.iter().map(|s| s.span_id.clone()))
            .collect();

        // DEBUG: Log span counts before filtering to identify what's being dropped
        if cfg!(debug_assertions) {
            let pre_filter_span_count: usize = glyph_positions.iter().map(|p| p.spans.len()).sum();
            let glyph_positions_span_ids: std::collections::HashSet<String> = glyph_positions
                .iter()
                .flat_map(|page| page.spans.iter().map(|s| format!("{:x}", s.span)))
                .collect();

            // Find spans that are in glyph_positions but NOT in glyph_maps (will be filtered out)
            let dropped_spans: Vec<&String> = glyph_positions_span_ids
                .iter()
                .filter(|id| !span_ids.contains(*id))
                .collect();

            if !dropped_spans.is_empty() || page_filter.is_some() {
                console_log(format!(
                    "[incr_compile] SPAN FILTER DEBUG: glyphMaps has {} unique spans, glyphPositions has {} unique spans, dropping {} spans: {:?}",
                    span_ids.len(),
                    glyph_positions_span_ids.len(),
                    dropped_spans.len(),
                    dropped_spans.iter().take(10).collect::<Vec<_>>()
                ));
            }
        }

        // ==========================================================================
        // CARMACK DEBUG ASSERTION: Per-page span parity check BEFORE filtering
        // This catches divergence between collect_glyph_maps and typst2vec at the source.
        // ==========================================================================
        #[cfg(debug_assertions)]
        {
            use std::collections::HashSet;
            let all_pages: HashSet<u32> = glyph_maps.iter().map(|p| p.page)
                .chain(glyph_positions.iter().map(|p| p.page))
                .collect();

            for page_num in all_pages {
                let gm_page = glyph_maps.iter().find(|p| p.page == page_num);
                let gp_page = glyph_positions.iter().find(|p| p.page == page_num);

                let gm_span_ids: HashSet<String> = gm_page
                    .map(|p| p.spans.iter().map(|s| s.span_id.to_lowercase()).collect())
                    .unwrap_or_default();
                let gp_span_ids: HashSet<String> = gp_page
                    .map(|p| p.spans.iter().map(|s| format!("{:x}", s.span).to_lowercase()).collect())
                    .unwrap_or_default();

                let in_gm_not_gp: Vec<_> = gm_span_ids.difference(&gp_span_ids).cloned().collect();
                let in_gp_not_gm: Vec<_> = gp_span_ids.difference(&gm_span_ids).cloned().collect();

                if !in_gm_not_gp.is_empty() || !in_gp_not_gm.is_empty() {
                    console_log(format!(
                        "[SPAN_PARITY_VIOLATION] Page {}: glyphMaps has {} unique spans, glyphPositions has {} unique spans",
                        page_num, gm_span_ids.len(), gp_span_ids.len()
                    ));
                    if !in_gm_not_gp.is_empty() {
                        console_log(format!(
                            "[SPAN_PARITY_VIOLATION] Page {}: IN glyphMaps BUT NOT glyphPositions ({} spans): {:?}",
                            page_num, in_gm_not_gp.len(), in_gm_not_gp.iter().take(10).collect::<Vec<_>>()
                        ));
                    }
                    if !in_gp_not_gm.is_empty() {
                        console_log(format!(
                            "[SPAN_PARITY_VIOLATION] Page {}: IN glyphPositions BUT NOT glyphMaps ({} spans): {:?}",
                            page_num, in_gp_not_gm.len(), in_gp_not_gm.iter().take(10).collect::<Vec<_>>()
                        ));
                    }
                    // In debug builds, panic to surface the issue immediately
                    // Comment out the panic for now to gather data, uncomment once fix is ready
                    // panic!(
                    //     "SPAN_PARITY_VIOLATION: Page {} has {} spans in glyphMaps not in glyphPositions, {} spans in glyphPositions not in glyphMaps",
                    //     page_num, in_gm_not_gp.len(), in_gp_not_gm.len()
                    // );
                }
            }
        }

        let filtered_glyph_positions = glyph_positions
            .into_iter()
            .map(|mut page| {
                let before = page.spans.len();
                page.spans.retain(|s| span_ids.contains(&format!("{:x}", s.span)));
                let after = page.spans.len();
                if before != after {
                    console_log(format!(
                        "[incr_compile] Page {} filtered: {} -> {} spans ({} dropped)",
                        page.page, before, after, before - after
                    ));
                }
                page
            })
            .collect::<Vec<_>>();

        let post_filter_span_count: usize = filtered_glyph_positions.iter().map(|p| p.spans.len()).sum();
        if pre_filter_span_count != post_filter_span_count {
            console_log(format!(
                "[incr_compile] TOTAL SPANS FILTERED: {} -> {} ({} dropped)",
                pre_filter_span_count, post_filter_span_count, pre_filter_span_count - post_filter_span_count
            ));
        }

        // Serialize glyph maps for accurate click positioning
        let glyph_maps_js = match serde_wasm_bindgen::to_value(&glyph_maps) {
            Ok(v) => {
                console_log(format!(
                    "[incr_compile] Serialized glyph maps: {} pages, is_null={}",
                    glyph_maps.len(),
                    v.is_null()
                ));
                v
            }
            Err(e) => {
                console_log(format!("[incr_compile] ERROR serializing glyph maps: {:?}", e));
                JsValue::NULL
            }
        };

        // Serialize glyph positions from typst2vec lowering (page-space x positions).
        let glyph_positions_js =
            serde_wasm_bindgen::to_value(&filtered_glyph_positions).unwrap_or(JsValue::NULL);

        // Collect syntax nodes for editor protection (headings, lists, math, etc.)
        let syntax_nodes = self.collect_syntax_nodes();
        let syntax_nodes_js = serde_wasm_bindgen::to_value(&syntax_nodes).unwrap_or(JsValue::NULL);

        // Filter layout map by page if filter is active
        let layout_map = if let Some(filter) = &page_filter {
            delta.layout_map.iter()
                .filter(|lp| filter.contains(&lp.page))
                .cloned()
                .collect::<Vec<_>>()
        } else {
            delta.layout_map.clone()
        };

        // Apply the SAME span_ids filter to layoutMap that we apply to glyphPositions
        // This ensures layoutMap and glyphPositions have matching spans, preventing
        // orphan spans (e.g., generated content) that have no glyph data from reaching the client
        let filtered_layout_map = layout_map
            .into_iter()
            .map(|mut page| {
                page.spans.retain(|s| span_ids.contains(&format!("{:x}", s.span)));
                page
            })
            .collect::<Vec<_>>();

        let layout_map_js =
            serde_wasm_bindgen::to_value(&filtered_layout_map).unwrap_or(JsValue::NULL);

        // Log filtering info
        if let Some(filter) = &page_filter {
            console_log(format!(
                "[incr_compile] Page filter active: {} pages requested, {} total pages. Returning {} glyph_positions, {} layout_map entries",
                filter.len(),
                total_page_count,
                filtered_glyph_positions.len(),
                filtered_layout_map.len()
            ));

            // CARMACK: Diff span IDs between glyphMaps and glyphPositions BEFORE JS serialization
            for page_num in filter.iter() {
                let gm_page = glyph_maps.iter().find(|p| p.page == *page_num);
                let gp_page = filtered_glyph_positions.iter().find(|p| p.page == *page_num);

                let gm_span_ids: HashSet<String> = gm_page
                    .map(|p| p.spans.iter().map(|s| s.span_id.clone()).collect())
                    .unwrap_or_default();
                let gp_span_ids: HashSet<String> = gp_page
                    .map(|p| p.spans.iter().map(|s| format!("{:x}", s.span)).collect())
                    .unwrap_or_default();

                let in_gm_not_gp: Vec<_> = gm_span_ids.difference(&gp_span_ids).collect();
                let in_gp_not_gm: Vec<_> = gp_span_ids.difference(&gm_span_ids).collect();

                if !in_gm_not_gp.is_empty() || !in_gp_not_gm.is_empty() {
                    console_log(format!("[SPAN_MISMATCH_RUST] Page {}:", page_num));
                    if !in_gm_not_gp.is_empty() {
                        let sample: Vec<_> = in_gm_not_gp.iter().take(5).collect();
                        console_log(format!("  In glyphMaps but NOT glyphPositions ({}): {:?}", in_gm_not_gp.len(), sample));
                    }
                    if !in_gp_not_gm.is_empty() {
                        let sample: Vec<_> = in_gp_not_gm.iter().take(5).collect();
                        console_log(format!("  In glyphPositions but NOT glyphMaps ({}): {:?}", in_gp_not_gm.len(), sample));
                    }
                }
            }
        }

        Ok(if diagnostics_format != 0 {
            console_log(format!("[incr_compile] Building result object with diagnostics_format={}", diagnostics_format));
            let result = js_sys::Object::new();
            js_sys::Reflect::set(&result, &"result".into(), &v)?;
            js_sys::Reflect::set(&result, &"spanRanges".into(), &span_ranges_js)?;
            js_sys::Reflect::set(&result, &"glyphMaps".into(), &glyph_maps_js)?;
            js_sys::Reflect::set(&result, &"glyphPositions".into(), &glyph_positions_js)?;
            js_sys::Reflect::set(&result, &"syntaxNodes".into(), &syntax_nodes_js)?;
            js_sys::Reflect::set(&result, &"layoutMap".into(), &layout_map_js)?;
            // Always include total page count so main thread knows document size
            js_sys::Reflect::set(&result, &"totalPageCount".into(), &JsValue::from(total_page_count))?;
            // Include filtered pages array if filter was active
            if let Some(filter) = &page_filter {
                let filtered_pages: Vec<u32> = filter.iter().copied().collect();
                let filtered_pages_js = serde_wasm_bindgen::to_value(&filtered_pages).unwrap_or(JsValue::NULL);
                js_sys::Reflect::set(&result, &"filteredPages".into(), &filtered_pages_js)?;
            }
            console_log(format!("[incr_compile] Result object created, glyphMaps set (value is_null={})", glyph_maps_js.is_null()));
            result.into()
        } else {
            console_log("[incr_compile] diagnostics_format is 0, returning raw delta (no glyphMaps)".to_string());
            // For backwards compatibility, just return delta if no diag format
            v
        })
    }

    /// Extract glyph data (maps + positions) for specific pages from the cached document.
    /// This is a pure extraction function - no incremental delta, no state changes.
    /// Used for hydration when scrolling to pages not in the initial viewport.
    ///
    /// This decouples cursor data extraction from the incremental render delta,
    /// allowing hydration to work even when the document hasn't changed.
    #[cfg(feature = "incr")]
    pub fn extract_glyph_data(
        &self,
        state: &IncrServer,
        pages: &JsValue,
    ) -> Result<JsValue, JsValue> {
        // Parse pages array
        let pages_array: Vec<u32> = if pages.is_null() || pages.is_undefined() {
            return Ok(JsValue::NULL);
        } else {
            serde_wasm_bindgen::from_value(pages.clone())
                .map_err(|e| JsValue::from_str(&format!("Invalid pages array: {}", e)))?
        };

        if pages_array.is_empty() {
            return Ok(JsValue::NULL);
        }

        let page_set: HashSet<u32> = pages_array.into_iter().collect();

        // Get cached doc from IncrServer
        let Some(doc) = state.cached_doc() else {
            console_log("[extract_glyph_data] No cached document".to_string());
            return Ok(JsValue::NULL);
        };

        if cfg!(debug_assertions) {
            console_log(format!(
                "[extract_glyph_data] Extracting glyph data for pages {:?} from cached doc with {} pages",
                page_set, doc.pages.len()
            ));
        }

        // Extract glyphMaps (byte offsets) using existing logic
        let glyph_maps = self.collect_glyph_maps(doc, Some(&page_set));

        // Extract glyphPositions (X/Y coordinates) using IncrServer's new method
        let Some((layout_map, glyph_positions)) = state.extract_glyph_positions_for_pages(&page_set) else {
            console_log("[extract_glyph_data] Failed to extract glyph positions".to_string());
            return Ok(JsValue::NULL);
        };

        // Filter glyphPositions to only include spans that have byte offsets in glyphMaps.
        // This ensures parity and prevents orphan spans (generated content without source mapping).
        let span_ids: std::collections::HashSet<String> = glyph_maps
            .iter()
            .flat_map(|page| page.spans.iter().map(|s| s.span_id.clone()))
            .collect();

        let filtered_glyph_positions: Vec<_> = glyph_positions
            .into_iter()
            .map(|mut page| {
                page.spans.retain(|s| span_ids.contains(&format!("{:x}", s.span)));
                page
            })
            .collect();

        // Similarly filter layoutMap to keep coherence
        let filtered_layout_map: Vec<_> = layout_map
            .into_iter()
            .map(|mut page| {
                page.spans.retain(|s| span_ids.contains(&format!("{:x}", s.span)));
                page
            })
            .collect();

        if cfg!(debug_assertions) {
            console_log(format!(
                "[extract_glyph_data] Extracted {} glyphMaps pages, {} glyphPositions pages (filtered), {} layoutMap pages (filtered)",
                glyph_maps.len(), filtered_glyph_positions.len(), filtered_layout_map.len()
            ));
        }

        // Serialize to JS
        let result = js_sys::Object::new();

        let glyph_maps_js = serde_wasm_bindgen::to_value(&glyph_maps)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize glyphMaps: {}", e)))?;
        js_sys::Reflect::set(&result, &"glyphMaps".into(), &glyph_maps_js)?;

        let glyph_positions_js = serde_wasm_bindgen::to_value(&filtered_glyph_positions)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize glyphPositions: {}", e)))?;
        js_sys::Reflect::set(&result, &"glyphPositions".into(), &glyph_positions_js)?;

        let layout_map_js = serde_wasm_bindgen::to_value(&filtered_layout_map)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize layoutMap: {}", e)))?;
        js_sys::Reflect::set(&result, &"layoutMap".into(), &layout_map_js)?;

        Ok(result.into())
    }

    /// Resolve span IDs from delta to byte ranges using the world.
    /// This walks the main source file's AST and resolves each span to byte offsets.
    #[cfg(feature = "incr")]
    fn resolve_span_ranges(&self, _delta_bytes: &[u8]) -> Vec<(String, u32, usize, usize)> {
        use ::typst::World;

        let mut result = Vec::new();
        let world = &self.graph.snap.world;

        // Get the main source file
        let main_id = world.main();
        if let Ok(source) = world.source(main_id) {
            let file_id = main_id.into_raw().get() as u32;
            let root = source.root();
            Self::collect_spans_from_node(root, &source, file_id, &mut result);
        }

        result
    }

    #[cfg(feature = "incr")]
    fn collect_spans_from_node(
        node: &::typst::syntax::SyntaxNode,
        source: &::typst::syntax::Source,
        file_id: u32,
        result: &mut Vec<(String, u32, usize, usize)>,
    ) {
        use std::num::NonZeroU64;

        let span = node.span();
        if !span.is_detached() {
            // Use source.range(span) instead of world.range(span) to avoid locking
            if let Some(range) = source.range(span) {
                // Convert span to hex string like the renderer expects
                if let Some(raw) = NonZeroU64::new(span.into_raw().get()) {
                    let span_hex = format!("{:x}", raw.get());
                    result.push((span_hex, file_id, range.start, range.end));
                }
            }
        }

        // Recurse into children
        for child in node.children() {
            Self::collect_spans_from_node(child, source, file_id, result);
        }
    }

    /// Collect syntax nodes for editor protection.
    /// Identifies structural elements (headings, lists, math) with marker boundaries.
    #[cfg(feature = "incr")]
    fn collect_syntax_nodes(&self) -> Vec<SyntaxNodeInfo> {
        use ::typst::World;

        let mut result = Vec::new();
        let world = &self.graph.snap.world;

        let main_id = world.main();
        if let Ok(source) = world.source(main_id) {
            let root = source.root();
            Self::collect_structural_nodes(root, &source, &mut result);
        }

        result
    }

    #[cfg(feature = "incr")]
    fn collect_structural_nodes(
        node: &::typst::syntax::SyntaxNode,
        source: &::typst::syntax::Source,
        result: &mut Vec<SyntaxNodeInfo>,
    ) {
        use ::typst::syntax::SyntaxKind;

        let kind = node.kind();

        // Only collect structural nodes that need protection
        let dominated_content = matches!(
            kind,
            SyntaxKind::Heading
                | SyntaxKind::ListItem
                | SyntaxKind::EnumItem
                | SyntaxKind::TermItem
                | SyntaxKind::Equation
                | SyntaxKind::Strong
                | SyntaxKind::Emph
                | SyntaxKind::Raw
                | SyntaxKind::FuncCall
        );

        if dominated_content {
            if let Some(range) = source.range(node.span()) {
                let boundaries = Self::find_marker_boundaries(node, source);

                // Generate span ID from the node's span
                let span_id = format!("{:x}", node.span().into_raw().get());

                // Ensure content_end is set even for empty nodes:
                // - If closing marker exists, content_end should be set by find_marker_boundaries
                // - Otherwise, use content_start (empty content) or marker_end (no content at all)
                let content_end = boundaries.content_end
                    .or(boundaries.content_start)
                    .or(boundaries.marker_end);

                result.push(SyntaxNodeInfo {
                    kind: format!("{:?}", kind),
                    span_id,
                    start: range.start,
                    end: range.end,
                    marker_start: boundaries.marker_start,
                    marker_end: boundaries.marker_end,
                    content_start: boundaries.content_start,
                    content_end,
                    closing_marker_start: boundaries.closing_marker_start,
                    closing_marker_end: boundaries.closing_marker_end,
                    depth: boundaries.depth,
                });
            }
        }

        // Always recurse to find nested structures
        for child in node.children() {
            Self::collect_structural_nodes(child, source, result);
        }
    }

    #[cfg(feature = "incr")]
    fn find_marker_boundaries(
        node: &::typst::syntax::SyntaxNode,
        source: &::typst::syntax::Source,
    ) -> MarkerBoundaries {
        use ::typst::syntax::SyntaxKind;

        let mut result = MarkerBoundaries {
            marker_start: None,
            marker_end: None,
            content_start: None,
            content_end: None,
            closing_marker_start: None,
            closing_marker_end: None,
            depth: None,
        };

        let node_kind = node.kind();

        // Track whether we've seen the opening marker (for paired delimiters)
        let mut seen_opening_marker = false;
        // Track last content position for content_end
        let mut last_content_end: Option<usize> = None;

        for child in node.children() {
            let child_kind = child.kind();
            if let Some(range) = source.range(child.span()) {
                match child_kind {
                    // Heading marker: "=", "==", etc. (no closing marker)
                    SyntaxKind::HeadingMarker => {
                        result.marker_start = Some(range.start);
                        result.marker_end = Some(range.end);
                        result.depth = Some(range.end - range.start);
                        seen_opening_marker = true;
                    }
                    // List/enum/term markers (no closing marker)
                    SyntaxKind::ListMarker | SyntaxKind::EnumMarker | SyntaxKind::TermMarker => {
                        result.marker_start = Some(range.start);
                        result.marker_end = Some(range.end);
                        seen_opening_marker = true;
                    }
                    // Math delimiters ($) - paired
                    SyntaxKind::Dollar => {
                        if !seen_opening_marker {
                            result.marker_start = Some(range.start);
                            result.marker_end = Some(range.end);
                            seen_opening_marker = true;
                        } else {
                            // This is the closing $
                            result.closing_marker_start = Some(range.start);
                            result.closing_marker_end = Some(range.end);
                            // Content ends where closing marker starts
                            if result.content_end.is_none() {
                                result.content_end = Some(range.start);
                            }
                        }
                    }
                    // Strong (*) markers - paired
                    SyntaxKind::Star => {
                        if node_kind == SyntaxKind::Strong {
                            if !seen_opening_marker {
                                result.marker_start = Some(range.start);
                                result.marker_end = Some(range.end);
                                seen_opening_marker = true;
                            } else {
                                result.closing_marker_start = Some(range.start);
                                result.closing_marker_end = Some(range.end);
                                if result.content_end.is_none() {
                                    result.content_end = Some(range.start);
                                }
                            }
                        }
                    }
                    // Emph (_) markers - paired
                    SyntaxKind::Underscore => {
                        if node_kind == SyntaxKind::Emph {
                            if !seen_opening_marker {
                                result.marker_start = Some(range.start);
                                result.marker_end = Some(range.end);
                                seen_opening_marker = true;
                            } else {
                                result.closing_marker_start = Some(range.start);
                                result.closing_marker_end = Some(range.end);
                                if result.content_end.is_none() {
                                    result.content_end = Some(range.start);
                                }
                            }
                        }
                    }
                    // Raw delimiters (`) - can be single or triple
                    SyntaxKind::RawDelim => {
                        if !seen_opening_marker {
                            result.marker_start = Some(range.start);
                            result.marker_end = Some(range.end);
                            seen_opening_marker = true;
                        } else {
                            result.closing_marker_start = Some(range.start);
                            result.closing_marker_end = Some(range.end);
                            if result.content_end.is_none() {
                                result.content_end = Some(range.start);
                            }
                        }
                    }
                    // FuncCall: identifier is the "marker", args are content
                    SyntaxKind::Ident if node_kind == SyntaxKind::FuncCall => {
                        if result.marker_start.is_none() {
                            result.marker_start = Some(range.start);
                            result.marker_end = Some(range.end);
                            seen_opening_marker = true;
                        }
                    }
                    // Skip whitespace for content tracking
                    SyntaxKind::Space | SyntaxKind::Linebreak | SyntaxKind::Parbreak => {}
                    // Track content region
                    _ => {
                        if seen_opening_marker {
                            if result.content_start.is_none() {
                                result.content_start = Some(range.start);
                            }
                            last_content_end = Some(range.end);
                        }
                    }
                }
            }
        }

        // For nodes without closing markers (headings, lists), content_end is the last content position
        if result.closing_marker_start.is_none() && last_content_end.is_some() {
            result.content_end = last_content_end;
        }

        result
    }

    /// Collect line boxes from the laid-out document.
    /// Detects actual line frames using content_hint (set by inline layout),
    /// rather than grouping text items by Y proximity.
    #[cfg(feature = "incr")]
    fn collect_line_boxes(&self, doc: &TypstPagedDocument) -> Vec<LineBox> {
        use ::typst::World;

        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // console_log(format!(
        //     "[collect_line_boxes] Starting, {} pages",
        //     doc.pages.len()
        // ));

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source = match world.source(main_id) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut lines: Vec<LineBox> = Vec::new();

        for (page_idx, page) in doc.pages.iter().enumerate() {
            let page_hint = page.frame.content_hint();
            // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
            // console_log(format!(
            //     "[collect_line_boxes] Page {} frame: size=({:.2}, {:.2})pt, hint=0x{:02x}, items={}",
            //     page_idx,
            //     page.frame.width().to_pt(),
            //     page.frame.height().to_pt(),
            //     page_hint as u32,
            //     page.frame.items().len()
            // ));

            // DEBUG: debug_print_frame_tree DISABLED - was printing entire frame tree recursively
            // Self::debug_print_frame_tree(&page.frame, 0, 5);

            Self::collect_lines_from_frame(
                page_idx as u32,
                &page.frame,
                TransformState::identity(),
                &source,
                &mut lines,
            );
        }

        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // console_log(format!(
        //     "[collect_line_boxes] Found {} line boxes total",
        //     lines.len()
        // ));

        // Sort by page, then by Y position (top)
        lines.sort_by(|a, b| {
            a.page.cmp(&b.page).then_with(|| {
                a.top
                    .partial_cmp(&b.top)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
        });

        // Ensure minimum line height (12pt)
        const MIN_LINE_HEIGHT: f32 = 12.0;
        for line in &mut lines {
            if line.height < MIN_LINE_HEIGHT {
                line.height = MIN_LINE_HEIGHT;
            }
        }

        lines
    }

    /// Collect glyph maps from the laid-out document.
    /// For each text item, extract the per-glyph byte offsets from the span data.
    /// This enables accurate click-to-cursor positioning.
    ///
    /// If `page_filter` is Some, only collect glyph maps for pages in the filter set.
    /// This enables viewport-aware compilation where only visible pages are processed.
    #[cfg(feature = "incr")]
    fn collect_glyph_maps(&self, doc: &TypstPagedDocument, page_filter: Option<&HashSet<u32>>) -> Vec<PageGlyphMap> {
        use ::typst::World;

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source = match world.source(main_id) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut page_maps: Vec<PageGlyphMap> = Vec::new();

        for (page_idx, page) in doc.pages.iter().enumerate() {
            // Skip pages not in the filter set (if filter is active)
            if let Some(filter) = page_filter {
                if !filter.contains(&(page_idx as u32)) {
                    continue;
                }
            }

            let mut spans: Vec<SpanGlyphMap> = Vec::new();

            Self::collect_glyphs_from_frame(&page.frame, &source, &mut spans);

            if !spans.is_empty() {
                // BUGFIX: Merge duplicate span entries into single entries per unique span_id.
                // This matches typst2vec.rs behavior where each span has one entry per run,
                // but we were creating multiple entries when spans appeared in multiple
                // FrameItem::Text elements (e.g., heading marker + heading text).
                //
                // Group by span_id and concatenate byte_offsets for each occurrence.
                use std::collections::HashMap;
                let mut merged: HashMap<String, Vec<usize>> = HashMap::new();
                for span in spans {
                    merged.entry(span.span_id)
                        .or_default()
                        .extend(span.byte_offsets);
                }
                let merged_spans: Vec<SpanGlyphMap> = merged.into_iter()
                    .map(|(span_id, byte_offsets)| SpanGlyphMap { span_id, byte_offsets })
                    .collect();

                page_maps.push(PageGlyphMap {
                    page: page_idx as u32,
                    version: 1, // Increment on each compile if caching
                    spans: merged_spans,
                });
            }
        }

        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // console_log(format!(
        //     "[collect_glyph_maps] Collected {} pages with glyph maps (filter={:?})",
        //     page_maps.len(),
        //     page_filter.map(|f| f.len())
        // ));

        page_maps
    }

    /// Recursively collect glyph byte offsets from text items in a frame.
    ///
    /// IMPORTANT: When text wraps to multiple lines, Typst creates multiple
    /// FrameItem::Text entries with the SAME span_id but at different Y positions.
    /// The client groups these by spanId into "chunks" for proper cursor placement.
    ///
    /// SYNTHETIC SPAN HANDLING (Buffer-and-Flush):
    /// Detached spans (list markers, auto-numbers, bullets) have no source mapping.
    /// We use a two-level strategy:
    /// 1. Within a text item: Find an "anchor" glyph (first attached) and remap
    ///    detached siblings to that anchor.
    /// 2. Across text items: Buffer purely synthetic text items and flush them
    ///    when we encounter a text item with a valid span. This handles isolated
    ///    bullets/markers that come as separate FrameItem::Text entries.
    #[cfg(feature = "incr")]
    fn collect_glyphs_from_frame(
        frame: &::typst::layout::Frame,
        source: &::typst::syntax::Source,
        result: &mut Vec<SpanGlyphMap>,
    ) {
        use ::typst::layout::FrameItem;
        use std::num::NonZeroU64;

        // Buffer for purely synthetic text items (no internal anchor)
        // Each entry: (glyph_count, byte_offsets placeholder)
        // We'll assign them a span_id when we find the next valid text item
        let mut synthetic_buffer: Vec<Vec<usize>> = Vec::new();

        // Last seen valid span info for fallback (if frame ends with synthetics)
        let mut last_valid_span: Option<(String, usize)> = None; // (span_id, start_byte)

        for (_pos, item) in frame.items() {
            match item {
                FrameItem::Group(group) => {
                    // Recurse into groups - they have their own frame context
                    Self::collect_glyphs_from_frame(&group.frame, source, result);
                }
                FrameItem::Text(text) => {
                    // Find an "anchor" glyph - first attached (non-detached) glyph in this text item.
                    // Used to give synthetic (detached) glyphs a valid span ID.
                    //
                    // MATCH TYPST2VEC EXACTLY: typst2vec uses unwrap_or(0) and then skips id==0.
                    // We do the same - always get anchor_id, and use 0 for purely synthetic items.
                    let anchor_id: u64 = text.glyphs.iter()
                        .find(|g| !g.span.0.is_detached())
                        .map(|g| g.span.0.into_raw().get())
                        .unwrap_or(0);

                    // Skip purely synthetic text items (no anchor) - matches typst2vec's `if id != 0`
                    if anchor_id == 0 {
                        // Buffer for later assignment (existing behavior)
                        let mut byte_offsets: Vec<usize> = Vec::with_capacity(text.glyphs.len() + 1);
                        for _ in 0..=text.glyphs.len() {
                            byte_offsets.push(0);
                        }
                        synthetic_buffer.push(byte_offsets);
                        continue;
                    }

                    // Get anchor range for fallback byte offsets. If not resolvable, use 0..0,
                    // but still emit offsets (matching typst2vec behavior: id != 0 is sufficient).
                    let anchor_range = text.glyphs.iter()
                        .find(|g| !g.span.0.is_detached())
                        .and_then(|g| source.range(g.span.0))
                        .unwrap_or(0..0);

                    let anchor_span_id = format!("{:x}", anchor_id);

                    // FLUSH: Assign buffered synthetic items to the anchor span
                    for buffered_offsets in synthetic_buffer.drain(..) {
                        result.push(SpanGlyphMap {
                            span_id: anchor_span_id.clone(),
                            byte_offsets: buffered_offsets,
                        });
                    }

                    // Process this text item's glyphs - GROUP BY SPAN ID
                    // This matches typst2vec.rs behavior where each glyph uses its own span
                    // (or anchor for detached glyphs)
                    use std::collections::HashMap;
                    let mut per_span_offsets: HashMap<u64, Vec<usize>> = HashMap::new();
                    let mut last_resolved_byte: Option<usize> = None;
                    let mut last_span_id: Option<u64> = None;

                    for glyph in text.glyphs.iter() {
                        let glyph_span = glyph.span.0;
                        let local_offset = glyph.span.1 as usize;

                        // Determine span ID - matches typst2vec.rs logic exactly
                        let span_id = if glyph_span.is_detached() {
                            anchor_id  // Use anchor for detached
                        } else {
                            glyph_span.into_raw().get()  // Use own span for attached
                        };

                        // Prefer glyph's own range; fall back to anchor start; if still none,
                        // use last_resolved_byte+1 or 0. Do not skip the glyph.
                        let byte_pos = if !glyph_span.is_detached() {
                            if let Some(range) = source.range(glyph_span) {
                                let pos = range.start + local_offset;
                                last_resolved_byte = Some(pos);
                                pos
                            } else {
                                anchor_range.start
                            }
                        } else {
                            anchor_range.start
                        };

                        per_span_offsets.entry(span_id).or_default().push(byte_pos);
                        last_span_id = Some(span_id);
                    }

                    // Add end position to the last span
                    if let (Some(last_glyph), Some(last_id)) = (text.glyphs.last(), last_span_id) {
                        let last_span = last_glyph.span.0;
                        let last_local_offset = last_glyph.span.1 as usize;

                        let end_byte = if !last_span.is_detached() {
                            if let Some(range) = source.range(last_span) {
                                range.start + last_local_offset + 1
                            } else {
                                last_resolved_byte.map(|b| b + 1).unwrap_or(anchor_range.start + 1)
                            }
                        } else {
                            last_resolved_byte.map(|b| b + 1).unwrap_or(anchor_range.start + 1)
                        };

                        per_span_offsets.entry(last_id).or_default().push(end_byte);
                    }

                    // Emit one SpanGlyphMap per span
                    for (span_raw, byte_offsets) in per_span_offsets {
                        if !byte_offsets.is_empty() {
                            let span_id = format!("{:x}", span_raw);
                            result.push(SpanGlyphMap {
                                span_id,
                                byte_offsets,
                            });
                        }
                    }

                    // Update last_valid_span for end-of-frame fallback
                    last_valid_span = Some((anchor_span_id, anchor_range.start));
                }
                _ => {}
            }
        }

        // End of frame: flush any remaining buffered synthetics to last_valid_span
        if !synthetic_buffer.is_empty() {
            if let Some((span_id, start_byte)) = last_valid_span {
                for mut buffered_offsets in synthetic_buffer.drain(..) {
                    // Assign all placeholders to the last valid span's start byte
                    for offset in buffered_offsets.iter_mut() {
                        *offset = start_byte;
                    }
                    result.push(SpanGlyphMap {
                        span_id: span_id.clone(),
                        byte_offsets: buffered_offsets,
                    });
                }
            }
            // If no last_valid_span exists, we have a frame with only synthetic content
            // This is rare (e.g., a frame with only auto-generated page numbers)
            // We drop these since there's no source location to map to
        }
    }

    /// Debug: print frame tree to understand hierarchy
    #[cfg(feature = "incr")]
    fn debug_print_frame_tree(frame: &::typst::layout::Frame, depth: usize, max_depth: usize) {
        use ::typst::layout::FrameItem;

        if depth > max_depth {
            return;
        }

        let indent = "  ".repeat(depth);
        let hint = frame.content_hint();
        let hint_str = if hint != '\0' {
            format!(" [HINT=0x{:02x}='{}']", hint as u32, if hint.is_ascii_graphic() { hint } else { '?' })
        } else {
            String::new()
        };

        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // console_log(format!(
        //     "{}Frame h={:.1}pt items={}{}",
        //     indent,
        //     frame.height().to_pt(),
        //     frame.items().len(),
        //     hint_str
        // ));

        for (pos, item) in frame.items() {
            match item {
                FrameItem::Group(group) => {
                    let t = &group.transform;
                    let child_hint = group.frame.content_hint();
                    let child_hint_str = if child_hint != '\0' {
                        format!(" [HINT=0x{:02x}]", child_hint as u32)
                    } else {
                        String::new()
                    };

                    // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
                    // if depth < 3 || child_hint != '\0' {
                    //     console_log(format!(
                    //         "{}  Group @({:.1},{:.1}) t=({:.2},{:.2}){}",
                    //         indent,
                    //         pos.x.to_pt(),
                    //         pos.y.to_pt(),
                    //         t.tx.to_pt(),
                    //         t.ty.to_pt(),
                    //         child_hint_str
                    //     ));
                    // }

                    Self::debug_print_frame_tree(&group.frame, depth + 1, max_depth);
                }
                // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
                // FrameItem::Text(text) if depth < 3 => {
                //     console_log(format!(
                //         "{}  Text @({:.1},{:.1}) {} glyphs",
                //         indent,
                //         pos.x.to_pt(),
                //         pos.y.to_pt(),
                //         text.glyphs.len()
                //     ));
                // }
                _ => {}
            }
        }
    }

    /// Recursively collect line frames from the frame hierarchy.
    /// Line frames are identified by having content_hint != '\0' (set by commit()).
    /// This gives us actual line geometry from Typst's layout stage.
    /// Applies full transforms (scale + translation) for accurate positioning.
    ///
    /// IMPORTANT: Block containers may have content_hint set on them (from collect.rs:449),
    /// but they contain child line frames with more granular hints. We should prefer
    /// the innermost line frames to get accurate per-line byte ranges.
    ///
    /// UPDATE: We now use a hybrid approach. We traverse using hints, but at the leaf level,
    /// we verify the content geometrically (clustering Y-positions) to ensure we don't
    /// inadvertently emit a single LineBox for a multi-line block that masked itself as a line.
    #[cfg(feature = "incr")]
    pub(crate) fn collect_lines_from_frame(
        page: u32,
        frame: &::typst::layout::Frame,
        transform: TransformState,
        source: &::typst::syntax::Source,
        out: &mut Vec<LineBox>,
    ) {
        use ::typst::layout::FrameItem;

        // Check if this frame has content_hint set
        let hint = frame.content_hint();
        let is_line_candidate = hint != '\0';

        // Track how many line boxes we have before traversing children so we can tell
        // whether a candidate frame actually contains nested line frames.
        let before_children = out.len();
        // Track whether this frame has any direct text items (helps distinguish containers)
        let mut direct_text_count = 0usize;

        // Debug: log traversal to first line frame only
        static DEPTH: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        for (pos, item) in frame.items() {
            // Apply position offset within this frame (pre_translate like typst2vec)
            let child_transform = transform.pre_translate(pos.x.to_pt(), pos.y.to_pt());

            match item {
                FrameItem::Group(group) => {
                    let gt = &group.transform;

                    // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
                    // Debug: log the path to first line only
                    // if out.is_empty() {
                    //     let depth = DEPTH.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    //     if depth < 10 {
                    //         let has_hint = group.frame.content_hint() != '\0';
                    //         console_log(format!(
                    //             "[Traverse d={}] pos=({:.2}, {:.2})pt → ty={:.2}pt | gt=({:.4}, {:.2})pt | is_line={}",
                    //             depth,
                    //             pos.x.to_pt(),
                    //             pos.y.to_pt(),
                    //             child_transform.pos_y(),
                    //             gt.sy.get(),
                    //             gt.ty.to_pt(),
                    //             has_hint
                    //         ));
                    //     }
                    // }
                    let _ = gt; // silence unused warning

                    // Apply the group's transform (pre_concat like typst2vec)
                    let group_transform = child_transform.pre_concat(&group.transform);
                    Self::collect_lines_from_frame(
                        page,
                        &group.frame,
                        group_transform,
                        source,
                        out,
                    );
                }
                FrameItem::Text(_) => {
                    direct_text_count += 1;
                }
                _ => {}
            }
        }

        let child_lines_added = out.len() > before_children;

        if !is_line_candidate {
            return;
        }

        if child_lines_added {
            // This frame marks a block, but nested frames contained the real lines.
            // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
            // if out.len() < 3 {
            //     console_log(format!(
            //         "[LineFrame CONTAINER] hint='{}' (0x{:02x}) - used child line frames",
            //         if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
            //         hint as u32,
            //     ));
            // }
            return;
        }

        // No child line frames were found; treat this frame as the leaf candidate.
        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // if out.len() < 3 {
        //     console_log(format!(
        //         "[LineFrame LEAF] hint='{}' (0x{:02x}), transform.ty={:.2}pt",
        //         if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
        //         hint as u32,
        //         transform.pos_y()
        //     ));
        // }

        // Hybrid Approach: verify content geometrically.
        let mut text_infos = Vec::new();
        Self::collect_text_infos_from_frame(frame, transform, source, &mut text_infos);

        if text_infos.is_empty() {
            // No text collected. In tests we still want synthetic frames to emit.
            #[cfg(test)]
            {
                let baseline_y = transform.apply_to_point(0.0, frame.baseline().to_pt()).1 as f32;
                let top = transform.pos_y() as f32;
                let height = (frame.height().to_pt() * transform.scale_y()) as f32;
                out.push(LineBox {
                    page,
                    top,
                    height,
                    baseline: baseline_y,
                    start_byte: 0,
                    end_byte: 1,
                });
            }
            #[cfg(not(test))]
            {
                return;
            }
        }

        // Heuristic guard: if a hinted frame has NO direct text AND a very large span,
        // treat it as a container and skip emitting a line box to avoid overlaps.
        // (Title case: small span with direct text passes through.)
        if direct_text_count == 0 {
            let start_min = text_infos.iter().map(|i| i.start).min().unwrap_or(0);
            let end_max = text_infos.iter().map(|i| i.end).max().unwrap_or(0);
            if end_max > start_min {
                let span = end_max - start_min;
                if span > 500 {
                    if out.len() < 3 {
                        console_log(format!(
                            "[LineFrame DROP] hint='{}' span={} (no direct text) at ty={:.2}pt",
                            if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
                            span,
                            transform.pos_y()
                        ));
                    }
                    return;
                }
            }
        }

        // Sort by baseline Y to enable clustering
        text_infos.sort_by(|a, b| a.baseline.partial_cmp(&b.baseline).unwrap());

        // Determine tolerance based on max glyph height in this frame
        // (handle large titles vs small text dynamically)
        let max_height = text_infos
            .iter()
            .map(|i| (i.bottom - i.top) as f32)
            .fold(0.0f32, f32::max);
        // Tolerance is 50% of max height, but at least 2.0pt
        let tolerance = (max_height as f64 * 0.5).max(2.0);

        // Cluster items by vertical position
        let mut clusters: Vec<Vec<TextItemInfo>> = Vec::new();
        for item in text_infos {
            if let Some(last_cluster) = clusters.last_mut() {
                // Check distance from the first item in the cluster (stable anchor)
                if (item.baseline - last_cluster[0].baseline).abs() <= tolerance {
                    last_cluster.push(item);
                    continue;
                }
            }
            clusters.push(vec![item]);
        }

        let cluster_count = clusters.len();
        // Log if we emit lines
        // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
        // if out.len() < 5 {
        //     console_log(format!(
        //         "[LineFrame LOOSE] hint='{}' (0x{:02x}) -> {} lines (tolerance={:.2})",
        //         if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
        //         hint as u32,
        //         cluster_count,
        //         tolerance
        //     ));
        // }

        // Emit LineBoxes for each cluster
        for (i, cluster) in clusters.into_iter().enumerate() {
            let start_byte = cluster.iter().map(|i| i.start).min().unwrap();
            let end_byte = cluster.iter().map(|i| i.end).max().unwrap();
            let min_y = cluster.iter().map(|i| i.top).fold(f64::INFINITY, f64::min);
            let max_y = cluster.iter().map(|i| i.bottom).fold(f64::NEG_INFINITY, f64::max);
            // Use the first item's baseline as the cluster baseline
            let baseline = cluster[0].baseline as f32;

            let height = (max_y - min_y) as f32;
            let top = min_y as f32;

            // DEBUG LOGGING DISABLED - was blocking main thread via console overhead
            // if out.len() < 5 {
            //     console_log(format!(
            //         "[LineBox #{}] Cluster {}/{} top={:.2}pt h={:.2}pt bytes={}..{}",
            //         out.len(),
            //         i + 1,
            //         cluster_count,
            //         top,
            //         height,
            //         start_byte,
            //         end_byte
            //     ));
            // }

            out.push(LineBox {
                page,
                top,
                height,
                baseline,
                start_byte,
                end_byte,
            });
        }
    }

    /// Test-only helper to collect line boxes without constructing a full compiler instance.
    #[cfg(all(test, feature = "incr"))]
    pub(crate) fn collect_lines_from_frame_for_test(
        page: u32,
        frame: &::typst::layout::Frame,
        transform: TransformState,
        source: &::typst::syntax::Source,
    ) -> Vec<LineBox> {
        let mut out = Vec::new();
        Self::collect_lines_from_frame(page, frame, transform, source, &mut out);
        out
    }

    /// Test-only helper to collect glyph maps without constructing a full compiler instance.
    #[cfg(all(test, feature = "incr"))]
    pub(crate) fn collect_glyphs_from_frame_for_test(
        frame: &::typst::layout::Frame,
        source: &::typst::syntax::Source,
    ) -> Vec<SpanGlyphMap> {
        let mut result = Vec::new();
        Self::collect_glyphs_from_frame(frame, source, &mut result);
        result
    }

    /// Collect text items from a frame (recursively) for geometric verification.
    ///
    /// Recurses into child groups (for styled text like bold/italic) but SKIPS
    /// any groups whose frames have `content_hint` set (those are child line frames
    /// that will be processed separately).
    #[cfg(feature = "incr")]
    fn collect_text_infos_from_frame(
        frame: &::typst::layout::Frame,
        transform: TransformState,
        source: &::typst::syntax::Source,
        out: &mut Vec<TextItemInfo>,
    ) {
        use ::typst::layout::FrameItem;

        for (pos, item) in frame.items() {
            let child_transform = transform.pre_translate(pos.x.to_pt(), pos.y.to_pt());

            match item {
                FrameItem::Text(text) => {
                    // Text item pos.y is the baseline
                    let (_, baseline) = child_transform.apply_to_point(0.0, 0.0);

                    // Calculate bounds from bbox (ink bounds)
                    let bbox = text.bbox();
                    let corners = [
                        (bbox.min.x.to_pt(), bbox.min.y.to_pt()),
                        (bbox.min.x.to_pt(), bbox.max.y.to_pt()),
                        (bbox.max.x.to_pt(), bbox.min.y.to_pt()),
                        (bbox.max.x.to_pt(), bbox.max.y.to_pt()),
                    ];

                    let mut min_y = f64::INFINITY;
                    let mut max_y = f64::NEG_INFINITY;

                    for (x, y) in corners {
                        let (_, ty) = child_transform.apply_to_point(x, y);
                        min_y = min_y.min(ty);
                        max_y = max_y.max(ty);
                    }

                    for glyph in &text.glyphs {
                        let span = glyph.span.0;
                        if !span.is_detached() {
                            if let Some(range) = source.range(span) {
                                out.push(TextItemInfo {
                                    baseline,
                                    top: min_y,
                                    bottom: max_y,
                                    start: range.start,
                                    end: range.end,
                                });
                            }
                        }
                    }
                }
                FrameItem::Group(group) => {
                    // Skip groups that are themselves line frames (have content_hint).
                    // Those will be processed as their own line boxes.
                    // Recurse into styling groups (no content_hint) to get styled text.
                    if group.frame.content_hint() == '\0' {
                        let group_transform = child_transform.pre_concat(&group.transform);
                        Self::collect_text_infos_from_frame(
                            &group.frame,
                            group_transform,
                            source,
                            out,
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn get_diag<D: TypstDocumentTrait + Send + Sync + 'static>(
        &self,
        diagnostics_format: u8,
    ) -> Result<JsValue, JsValue> {
        let diag = self.graph.compute::<TDiagnosticsTask<D>>()?;
        if diagnostics_format >= 2 {
            Ok(convert_diag(
                diag.diagnostics(),
                Some(&self.graph.snap.world),
                diag.error_cnt() > 0,
                diagnostics_format,
            ))
        } else if diag.error_cnt() > 0 {
            let diag = diag.diagnostics().collect::<Vec<_>>();
            return Err(format!("{diag:?}").into());
        } else {
            Ok(JsValue::UNDEFINED)
        }
    }

    fn do_compile_html(&mut self) -> Result<Option<Arc<TypstHtmlDocument>>, JsValue> {
        let g = &self.graph;
        let _ = g.provide::<HtmlCFlag>(Ok(FlagTask::flag(true)));
        Ok(g.shared_compile_html()?)
    }

    fn do_compile_paged(&mut self) -> Result<Option<Arc<TypstPagedDocument>>, JsValue> {
        let g = &self.graph;
        let _ = g.provide::<PagedCFlag>(Ok(FlagTask::flag(true)));
        Ok(g.shared_compile()?)
    }

    fn get_doc(&self, kind: u8) -> Result<Option<TypstDocument>, JsValue> {
        Ok(match kind {
            0 => self
                .get_doc_t::<TypstPagedDocument>()?
                .map(TypstDocument::Paged),
            1 => self
                .get_doc_t::<TypstHtmlDocument>()?
                .map(TypstDocument::Html),
            _ => return Err(error_once!("invalid kind", kind: kind).into()),
        })
    }

    fn get_doc_t<D: TypstDocumentTrait + Send + Sync + 'static>(
        &self,
    ) -> Result<Option<Arc<D>>, JsValue> {
        // todo: don't coupled me with compilation.
        self.graph
            .get::<CFlag<D>>()
            .ok_or_else(|| error_once!("document is not compiled"))??;
        Ok(self
            .graph
            .get::<OptionDocumentTask<D>>()
            .ok_or_else(|| error_once!("document did not compile"))??
            .as_ref()
            .clone())
    }
}

struct CompilationDiagnostics {
    errors: Option<EcoVec<typst::diag::SourceDiagnostic>>,
    warnings: Option<EcoVec<typst::diag::SourceDiagnostic>>,
}

impl CompilationDiagnostics {
    fn from_result<T>(result: &Option<Warned<SourceResult<T>>>) -> Self {
        let errors = result
            .as_ref()
            .and_then(|r| r.output.as_ref().map_err(|e| e.clone()).err());
        let warnings = result.as_ref().map(|r| r.warnings.clone());

        Self { errors, warnings }
    }
}

pub struct TDiagnosticsTask<D> {
    diag: CompilationDiagnostics,
    _phantom: std::marker::PhantomData<D>,
}

impl<F: CompilerFeat, D: typst::TypstDocumentTrait + Send + Sync + 'static> WorldComputable<F>
    for TDiagnosticsTask<D>
{
    type Output = Self;

    fn compute(graph: &Arc<WorldComputeGraph<F>>) -> Result<Self> {
        // let paged = graph.compute::<PagedCompilationTask>()?.clone();
        // let html = graph.compute::<HtmlCompilationTask>()?.clone();
        let diag = graph.compute::<CompilationTask<D>>()?;

        Ok(Self {
            diag: CompilationDiagnostics::from_result(&diag),
            _phantom: std::marker::PhantomData,
        })
    }
}

impl<D> TDiagnosticsTask<D> {
    pub fn error_cnt(&self) -> usize {
        self.diag.errors.as_ref().map_or(0, |e| e.len())
    }

    pub fn warning_cnt(&self) -> usize {
        self.diag.warnings.as_ref().map_or(0, |e| e.len())
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = &typst::diag::SourceDiagnostic> {
        self.diag
            .errors
            .iter()
            .chain(self.diag.warnings.iter())
            .flatten()
    }
}

// Convert the input pairs to a dictionary.
fn convert_inputs(inputs: &[js_sys::Array]) -> typst::foundations::Dict {
    inputs
        .iter()
        .map(|j| {
            (
                j.get(0).as_string().unwrap_or_default().into(),
                j.get(1).as_string().into_value(),
            )
        })
        .collect()
}

#[cfg(test)]
#[cfg(target_arch = "wasm32")]
mod tests {
    #![allow(clippy::await_holding_lock)]

    use reflexo_vec2svg::MultiVecDocument;
    use sha2::Digest;
    use typst_ts_test_common::web_artifact::get_corpus;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_test::*;

    use crate::builder::TypstCompilerBuilder;
    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    async fn get_source(name: &str) -> Vec<u8> {
        let array_buffer = get_corpus(format!("{}.typ", name)).await.unwrap();
        js_sys::Uint8Array::new(&array_buffer).to_vec()
    }

    async fn get_ir_artifact(name: &str) -> Vec<u8> {
        let array_buffer = get_corpus(format!("{}.artifact.sir.in", name))
            .await
            .unwrap();
        js_sys::Uint8Array::new(&array_buffer).to_vec()
    }

    fn hash_bytes<T: AsRef<[u8]>>(bytes: T) -> String {
        format!("sha256:{}", hex::encode(sha2::Sha256::digest(bytes)))
    }

    fn render_svg(artifact: &[u8]) -> String {
        let doc = MultiSvgDocument::from_slice(artifact);
        type UsingExporter = reflexo_vec2svg::SvgExporter<reflexo_vec2svg::SvgExportFeature>;

        let node = doc.layouts[0].unwrap_single();
        let view = node.pages(&doc.module).unwrap();
        UsingExporter::render_flat_svg(&doc.module, view.pages())
    }

    async fn render_test_template(point: &str, source: &[u8], artifact: &[u8]) {
        let window = web_sys::window().expect("should have a window in this context");
        let performance = window
            .performance()
            .expect("performance should be available");

        let mut compiler = TypstCompilerBuilder::new().unwrap();
        compiler.set_dummy_access_model().await.unwrap();
        let mut compiler = compiler.build().await.unwrap();
        let start = performance.now();
        if !compiler.add_source(
            &format!("/{point}.typ"),
            std::str::from_utf8(source).unwrap(),
            true,
        ) {
            panic!("Failed to add source {point}");
        }
        let end = performance.now();
        let time_used = end - start;

        let browser_artifact = compiler.compile(format!("/{point}.typ")).unwrap();

        let x_svg = render_svg(&browser_artifact);
        let y_svg = render_svg(artifact);

        let x_hash = hash_bytes(&x_svg);
        let y_hash = hash_bytes(&y_svg);

        use base64::Engine;
        let e = base64::engine::general_purpose::STANDARD;
        let x = web_sys::HtmlImageElement::new().unwrap();
        x.set_src(&format!("data:image/svg+xml;base64,{}", e.encode(x_svg)));
        x.set_attribute("style", "flex: 1;").unwrap();
        let y = web_sys::HtmlImageElement::new().unwrap();
        y.set_src(&format!("data:image/svg+xml;base64,{}", e.encode(y_svg)));
        y.set_attribute("style", "flex: 1;").unwrap();

        let div = window
            .document()
            .unwrap()
            .create_element("div")
            .unwrap()
            .dyn_into::<web_sys::HtmlElement>()
            .unwrap();

        div.set_attribute("style", "display block; border: 1px solid #000;")
            .unwrap();

        let title = window
            .document()
            .unwrap()
            .create_element("div")
            .unwrap()
            .dyn_into::<web_sys::HtmlElement>()
            .unwrap();

        title.set_inner_html(&format!(
            "{point} => {time_used:.3}ms, hash_cmp: {x_hash} v.s. {y_hash}",
        ));

        div.append_child(&title).unwrap();

        let cmp = window
            .document()
            .unwrap()
            .create_element("div")
            .unwrap()
            .dyn_into::<web_sys::HtmlElement>()
            .unwrap();
        cmp.set_attribute("style", "display: flex;").unwrap();

        cmp.append_child(&x).unwrap();
        cmp.append_child(&y).unwrap();

        div.append_child(&cmp).unwrap();

        let body = window.document().unwrap().body().unwrap();

        body.append_child(&div).unwrap();
    }

    async fn render_test_from_corpus(path: &str) {
        let point = path.replace('/', "_");
        let ir_point = format!("{}_artifact_ir", point);

        render_test_template(
            &ir_point,
            &get_source(path).await,
            &get_ir_artifact(path).await,
        )
        .await;
    }

    macro_rules! make_test_point {
        ($name:ident, $($path:literal),+ $(,)?) => {
            #[wasm_bindgen_test]
            async fn $name() {
                $(
                    render_test_from_corpus($path).await;
                )*
            }
        };
    }

    make_test_point!(test_render_math_main, "math/main");
    make_test_point!(test_render_math_undergradmath, "math/undergradmath");
}
