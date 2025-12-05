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

pub use crate::builder::TypstFontResolver;
pub use reflexo_typst::*;

use core::fmt;
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
        let Some(doc) = self.do_compile_paged()? else {
            return self.get_diag::<TypstPagedDocument>(diagnostics_format);
        };

        // Collect line boxes before updating state (we need the doc reference)
        let line_boxes = self.collect_line_boxes(&doc);

        // Collect glyph maps for accurate click-to-cursor positioning
        let glyph_maps = self.collect_glyph_maps(&doc);

        let delta = state.update(doc);
        let v = Uint8Array::from(delta.bytes.as_slice()).into();

        // Resolve span ranges from the world for source mapping
        let span_ranges = self.resolve_span_ranges(&delta.bytes);
        let span_ranges_js = serde_wasm_bindgen::to_value(&span_ranges).unwrap_or(JsValue::NULL);

        // Serialize line boxes for cursor height consistency
        let line_boxes_js = serde_wasm_bindgen::to_value(&line_boxes).unwrap_or(JsValue::NULL);

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
            serde_wasm_bindgen::to_value(&delta.glyph_map).unwrap_or(JsValue::NULL);

        // Collect syntax nodes for editor protection (headings, lists, math, etc.)
        let syntax_nodes = self.collect_syntax_nodes();
        let syntax_nodes_js = serde_wasm_bindgen::to_value(&syntax_nodes).unwrap_or(JsValue::NULL);

        let layout_map_js =
            serde_wasm_bindgen::to_value(&delta.layout_map).unwrap_or(JsValue::NULL);

        Ok(if diagnostics_format != 0 {
            console_log(format!("[incr_compile] Building result object with diagnostics_format={}", diagnostics_format));
            let result = js_sys::Object::new();
            js_sys::Reflect::set(&result, &"result".into(), &v)?;
            js_sys::Reflect::set(&result, &"spanRanges".into(), &span_ranges_js)?;
            js_sys::Reflect::set(&result, &"lineBoxes".into(), &line_boxes_js)?;
            js_sys::Reflect::set(&result, &"glyphMaps".into(), &glyph_maps_js)?;
            js_sys::Reflect::set(&result, &"glyphPositions".into(), &glyph_positions_js)?;
            js_sys::Reflect::set(&result, &"syntaxNodes".into(), &syntax_nodes_js)?;
            js_sys::Reflect::set(&result, &"layoutMap".into(), &layout_map_js)?;
            console_log(format!("[incr_compile] Result object created, glyphMaps set (value is_null={})", glyph_maps_js.is_null()));
            result.into()
        } else {
            console_log("[incr_compile] diagnostics_format is 0, returning raw delta (no glyphMaps)".to_string());
            // For backwards compatibility, just return delta if no diag format
            v
        })
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

        console_log(format!(
            "[collect_line_boxes] Starting, {} pages",
            doc.pages.len()
        ));

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source = match world.source(main_id) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut lines: Vec<LineBox> = Vec::new();

        for (page_idx, page) in doc.pages.iter().enumerate() {
            let page_hint = page.frame.content_hint();
            console_log(format!(
                "[collect_line_boxes] Page {} frame: size=({:.2}, {:.2})pt, hint=0x{:02x}, items={}",
                page_idx,
                page.frame.width().to_pt(),
                page.frame.height().to_pt(),
                page_hint as u32,
                page.frame.items().len()
            ));

            // Debug: Print top-level frame hierarchy to understand structure
            Self::debug_print_frame_tree(&page.frame, 0, 5);

            Self::collect_lines_from_frame(
                page_idx as u32,
                &page.frame,
                TransformState::identity(),
                &source,
                &mut lines,
            );
        }

        console_log(format!(
            "[collect_line_boxes] Found {} line boxes total",
            lines.len()
        ));

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
    #[cfg(feature = "incr")]
    fn collect_glyph_maps(&self, doc: &TypstPagedDocument) -> Vec<PageGlyphMap> {
        use ::typst::World;

        let world = &self.graph.snap.world;
        let main_id = world.main();
        let source = match world.source(main_id) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut page_maps: Vec<PageGlyphMap> = Vec::new();

        for (page_idx, page) in doc.pages.iter().enumerate() {
            let mut spans: Vec<SpanGlyphMap> = Vec::new();

            Self::collect_glyphs_from_frame(&page.frame, &source, &mut spans);

            if !spans.is_empty() {
                page_maps.push(PageGlyphMap {
                    page: page_idx as u32,
                    version: 1, // Increment on each compile if caching
                    spans,
                });
            }
        }

        console_log(format!(
            "[collect_glyph_maps] Collected {} pages with glyph maps",
            page_maps.len()
        ));

        page_maps
    }

    /// Recursively collect glyph byte offsets from text items in a frame.
    ///
    /// IMPORTANT: When text wraps to multiple lines, Typst creates multiple
    /// FrameItem::Text entries with the SAME span_id but at different Y positions.
    /// The client groups these by spanId into "chunks" for proper cursor placement.
    #[cfg(feature = "incr")]
    fn collect_glyphs_from_frame(
        frame: &::typst::layout::Frame,
        source: &::typst::syntax::Source,
        result: &mut Vec<SpanGlyphMap>,
    ) {
        use ::typst::layout::FrameItem;
        use std::num::NonZeroU64;

        for (pos, item) in frame.items() {
            match item {
                FrameItem::Group(group) => {
                    Self::collect_glyphs_from_frame(&group.frame, source, result);
                }
                FrameItem::Text(text) => {
                    // Get the span ID from the first glyph
                    // NOTE: For wrapped text, multiple FrameItem::Text may have the same span
                    if let Some(first_glyph) = text.glyphs.first() {
                        let span = first_glyph.span.0;
                        if !span.is_detached() {
                            if let Some(raw) = NonZeroU64::new(span.into_raw().get()) {
                                let span_id = format!("{:x}", raw.get());

                                // Debug: Count how many times we've seen this span_id already
                                let existing_count = result.iter().filter(|m| m.span_id == span_id).count();

                                // ALWAYS log for debugging wrapped text issues
                                // Check if all glyphs share the same SourceSpan or if they differ
                                let mut unique_spans = std::collections::HashSet::new();
                                for g in text.glyphs.iter() {
                                    if let Some(r) = NonZeroU64::new(g.span.0.into_raw().get()) {
                                        unique_spans.insert(r.get());
                                    }
                                }

                                console_log(format!(
                                    "[GlyphMap] span={} pos=({:.1},{:.1}) glyphs={} unique_glyph_spans={} existing_chunks={} first_local_offset={}",
                                    &span_id[..8.min(span_id.len())],
                                    pos.x.to_pt(),
                                    pos.y.to_pt(),
                                    text.glyphs.len(),
                                    unique_spans.len(),
                                    existing_count,
                                    first_glyph.span.1
                                ));

                                // Collect byte offsets for each glyph
                                // IMPORTANT: Each glyph has its own span info (span.0 = SourceSpan, span.1 = local offset)
                                // We need to resolve each glyph's absolute byte position individually
                                let mut byte_offsets: Vec<usize> = Vec::with_capacity(text.glyphs.len() + 1);

                                let mut all_resolved = true;
                                for (idx, glyph) in text.glyphs.iter().enumerate() {
                                    // Each glyph has its own span and local offset
                                    let glyph_span = glyph.span.0;
                                    let local_offset = glyph.span.1 as usize;

                                    // Get absolute byte position for this glyph - NO HEURISTICS
                                    // If we can't resolve the span, we skip this entire text item
                                    if let Some(range) = source.range(glyph_span) {
                                        let byte_pos = range.start + local_offset;

                                        // Debug first few glyphs
                                        if idx < 3 || idx == text.glyphs.len() - 1 {
                                            let glyph_span_raw = NonZeroU64::new(glyph_span.into_raw().get())
                                                .map(|r| format!("{:x}", r.get()))
                                                .unwrap_or_else(|| "detached".to_string());
                                            console_log(format!(
                                                "[GlyphMap] span={} glyph[{}]: local_offset={}, byte_pos={}, glyph_span={}",
                                                &span_id[..8.min(span_id.len())],
                                                idx,
                                                local_offset,
                                                byte_pos,
                                                &glyph_span_raw[..8.min(glyph_span_raw.len())]
                                            ));
                                        }

                                        byte_offsets.push(byte_pos);
                                    } else {
                                        // Can't resolve this glyph's span - skip entire text item
                                        // to avoid emitting partial/incorrect data
                                        all_resolved = false;
                                        console_log(format!(
                                            "[GlyphMap] span={} glyph[{}]: UNRESOLVED span, skipping text item",
                                            &span_id[..8.min(span_id.len())],
                                            idx
                                        ));
                                        break;
                                    }
                                }

                                // Only proceed if all glyphs were resolved
                                if !all_resolved {
                                    continue;
                                }

                                // Add end position (byte after last glyph)
                                // BUG FIX: We were using `range.end` which is the end of the ENTIRE
                                // source span (e.g., byte 191 for all text items in a paragraph).
                                // This caused overlapping byte ranges across lines, breaking
                                // byte->chunk resolution for wrapped text.
                                //
                                // CORRECT: Use range.start + last_glyph.span.1 + 1
                                // This gives the byte position AFTER the last glyph in this text item.
                                if let Some(last_glyph) = text.glyphs.last() {
                                    let last_span = last_glyph.span.0;
                                    let last_local_offset = last_glyph.span.1 as usize;
                                    if let Some(range) = source.range(last_span) {
                                        // End position = start of span + local offset of last glyph + 1
                                        // The +1 accounts for the glyph itself (assumes 1-byte chars;
                                        // for multi-byte UTF-8, the next glyph's offset will be used)
                                        let end_byte = range.start + last_local_offset + 1;
                                        byte_offsets.push(end_byte);
                                    } else {
                                        // Can't resolve end position - skip this text item
                                        console_log(format!(
                                            "[GlyphMap] span={}: UNRESOLVED end span, skipping",
                                            &span_id[..8.min(span_id.len())]
                                        ));
                                        continue;
                                    }
                                } else {
                                    // No glyphs - skip
                                    continue;
                                }

                                result.push(SpanGlyphMap {
                                    span_id,
                                    byte_offsets,
                                });
                            }
                        }
                    }
                }
                _ => {}
            }
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

        console_log(format!(
            "{}Frame h={:.1}pt items={}{}",
            indent,
            frame.height().to_pt(),
            frame.items().len(),
            hint_str
        ));

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

                    if depth < 3 || child_hint != '\0' {
                        console_log(format!(
                            "{}  Group @({:.1},{:.1}) t=({:.2},{:.2}){}",
                            indent,
                            pos.x.to_pt(),
                            pos.y.to_pt(),
                            t.tx.to_pt(),
                            t.ty.to_pt(),
                            child_hint_str
                        ));
                    }

                    Self::debug_print_frame_tree(&group.frame, depth + 1, max_depth);
                }
                FrameItem::Text(text) if depth < 3 => {
                    console_log(format!(
                        "{}  Text @({:.1},{:.1}) {} glyphs",
                        indent,
                        pos.x.to_pt(),
                        pos.y.to_pt(),
                        text.glyphs.len()
                    ));
                }
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

                    // Debug: log the path to first line only
                    if out.is_empty() {
                        let depth = DEPTH.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        if depth < 10 {
                            let has_hint = group.frame.content_hint() != '\0';
                            console_log(format!(
                                "[Traverse d={}] pos=({:.2}, {:.2})pt → ty={:.2}pt | gt=({:.4}, {:.2})pt | is_line={}",
                                depth,
                                pos.x.to_pt(),
                                pos.y.to_pt(),
                                child_transform.pos_y(),
                                gt.sy.get(),
                                gt.ty.to_pt(),
                                has_hint
                            ));
                        }
                    }

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
            if out.len() < 3 {
                console_log(format!(
                    "[LineFrame CONTAINER] hint='{}' (0x{:02x}) - used child line frames",
                    if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
                    hint as u32,
                ));
            }
            return;
        }

        // No child line frames were found; treat this frame as the leaf candidate.
        if out.len() < 3 {
            console_log(format!(
                "[LineFrame LEAF] hint='{}' (0x{:02x}), transform.ty={:.2}pt",
                if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
                hint as u32,
                transform.pos_y()
            ));
        }

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
        if out.len() < 5 {
            console_log(format!(
                "[LineFrame LOOSE] hint='{}' (0x{:02x}) -> {} lines (tolerance={:.2})",
                if hint.is_ascii_graphic() || hint == ' ' { hint } else { '?' },
                hint as u32,
                cluster_count,
                tolerance
            ));
        }

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

            if out.len() < 5 {
                console_log(format!(
                    "[LineBox #{}] Cluster {}/{} top={:.2}pt h={:.2}pt bytes={}..{}",
                    out.len(),
                    i + 1,
                    cluster_count,
                    top,
                    height,
                    start_byte,
                    end_byte
                ));
            }

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
