use std::collections::HashSet;
use std::sync::Arc;

use reflexo_typst::{TypstDocument, TypstPagedDocument};
use reflexo_typst2vec::incr::{IncrDocServer, PackedDelta};
use reflexo_typst2vec::layout::{PageGlyphPositions, PageLayout};
use reflexo_typst2vec::pass::IncrTypst2VecPass;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct IncrServer {
    inner: IncrDocServer,
    attach_debug_info: bool,
    /// Optional set of page indices to include in glyph map/positions/layout output.
    /// If None, all pages are included. If Some, only listed pages.
    /// This is reset after each incr_compile call via take().
    glyph_map_pages: Option<HashSet<u32>>,
    /// Cached document for on-demand glyph extraction (hydration).
    /// Updated on every successful compile.
    cached_doc: Option<Arc<TypstPagedDocument>>,
}

impl Default for IncrServer {
    fn default() -> Self {
        let mut this = Self {
            inner: IncrDocServer::default(),
            attach_debug_info: true,
            glyph_map_pages: None,
            cached_doc: None,
        };
        this.inner.set_should_attach_debug_info(true);
        this
    }
}

impl IncrServer {
    pub(crate) fn update(&mut self, doc: Arc<TypstPagedDocument>) -> PackedDelta {
        // Cache doc for on-demand extraction (hydration)
        self.cached_doc = Some(doc.clone());

        self.inner.pack_delta(&TypstDocument::Paged(doc))
    }

    /// Take the glyph_map_pages filter, resetting it to None.
    /// Called at the start of incr_compile to get the per-compile filter.
    pub(crate) fn take_glyph_map_pages(&mut self) -> Option<HashSet<u32>> {
        self.glyph_map_pages.take()
    }

    /// Extract glyph positions for specific pages from the cached document.
    /// This is a pure extraction - no incremental delta, no state changes.
    /// Used for hydration when scrolling to pages not in the initial viewport.
    ///
    /// Returns (layout_map, glyph_positions) for the requested pages.
    /// Only traverses the requested pages, not the entire document.
    pub(crate) fn extract_glyph_positions_for_pages(
        &self,
        pages: &HashSet<u32>,
    ) -> Option<(Vec<PageLayout>, Vec<PageGlyphPositions>)> {
        let doc = self.cached_doc.as_ref()?;

        // Create a temporary pass just for layout extraction
        // This doesn't modify any incremental state
        let mut pass = IncrTypst2VecPass::default();
        pass.spans.set_should_attach_debug_info(self.attach_debug_info);

        // Use filtered traversal - only processes requested pages, skips the rest
        let (layout_map, glyph_map) = pass.paged_filtered(doc, pages);

        Some((layout_map, glyph_map))
    }

    /// Get the cached document reference (for lib.rs to extract glyphMaps)
    pub(crate) fn cached_doc(&self) -> Option<&Arc<TypstPagedDocument>> {
        self.cached_doc.as_ref()
    }
}

#[wasm_bindgen]
impl IncrServer {
    pub fn set_attach_debug_info(&mut self, attach: bool) {
        self.attach_debug_info = attach;
        self.inner.set_should_attach_debug_info(attach);
    }

    /// Set which pages to include in glyph map/positions/layout output.
    /// Pass an empty array or null to clear the filter (include all pages).
    /// Pass specific page indices (0-indexed) to filter output to only those pages.
    /// The filter is automatically reset after each incr_compile call.
    #[wasm_bindgen(js_name = "set_glyph_map_pages")]
    pub fn set_glyph_map_pages(&mut self, pages: &JsValue) -> Result<(), JsValue> {
        if pages.is_null() || pages.is_undefined() {
            self.glyph_map_pages = None;
            return Ok(());
        }

        let pages_array: Vec<u32> = serde_wasm_bindgen::from_value(pages.clone())
            .map_err(|e| JsValue::from_str(&format!("Invalid pages array: {}", e)))?;

        if pages_array.is_empty() {
            self.glyph_map_pages = None;
        } else {
            self.glyph_map_pages = Some(pages_array.into_iter().collect());
        }

        Ok(())
    }

    pub fn current(&mut self) -> Option<Vec<u8>> {
        self.inner.pack_current()
    }

    pub fn reset(&mut self) {
        self.inner = IncrDocServer::default();
        self.inner
            .set_should_attach_debug_info(self.attach_debug_info);
        self.glyph_map_pages = None;
        self.cached_doc = None;
    }

    /// Extract glyph positions (X/Y coordinates) for specific pages from the cached document.
    /// This is a pure extraction - no incremental delta, no state changes.
    /// Used for hydration when scrolling to pages not in the initial viewport.
    ///
    /// Takes an array of page indices (0-indexed) and returns a JS object with:
    /// - glyphPositions: array of {page, spans: [{span, positions, glyph_spans, ...}]}
    /// - layoutMap: array of {page, spans: [{span, x, y, width, height}]}
    ///
    /// Returns null if no document is cached.
    #[wasm_bindgen(js_name = "extract_glyph_positions")]
    pub fn extract_glyph_positions_js(&self, pages: &JsValue) -> Result<JsValue, JsValue> {
        use js_sys::Object;

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

        // Extract glyph positions from cached doc
        let Some((layout_map, glyph_positions)) = self.extract_glyph_positions_for_pages(&page_set) else {
            return Ok(JsValue::NULL);
        };

        // Serialize to JS
        let result = Object::new();

        let glyph_positions_js = serde_wasm_bindgen::to_value(&glyph_positions)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize glyphPositions: {}", e)))?;
        js_sys::Reflect::set(&result, &"glyphPositions".into(), &glyph_positions_js)?;

        let layout_map_js = serde_wasm_bindgen::to_value(&layout_map)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize layoutMap: {}", e)))?;
        js_sys::Reflect::set(&result, &"layoutMap".into(), &layout_map_js)?;

        Ok(result.into())
    }

    /// Check if a document is cached (useful for debugging)
    #[wasm_bindgen(js_name = "has_cached_doc")]
    pub fn has_cached_doc(&self) -> bool {
        self.cached_doc.is_some()
    }

    /// Get the number of pages in the cached document (useful for debugging)
    #[wasm_bindgen(js_name = "cached_page_count")]
    pub fn cached_page_count(&self) -> u32 {
        self.cached_doc.as_ref().map(|d| d.pages.len() as u32).unwrap_or(0)
    }
}
