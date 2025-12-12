use std::collections::HashSet;
use std::sync::Arc;

use reflexo_typst::{TypstDocument, TypstPagedDocument};
use reflexo_typst2vec::incr::{IncrDocServer, PackedDelta};
use reflexo_typst2vec::layout::{PageGlyphPositions, PageLayout};
#[cfg(feature = "incr-glyph-maps")]
use reflexo_typst2vec::layout::PageGlyphOffsets;
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
    /// Cached span ranges for the cached document.
    /// Populated once per successful incr_compile and reused for hydration.
    #[cfg(feature = "incr-glyph-maps")]
    cached_span_ranges: Option<Vec<(String, u32, usize, usize)>>,
}

impl Default for IncrServer {
    fn default() -> Self {
        let mut this = Self {
            inner: IncrDocServer::default(),
            attach_debug_info: true,
            glyph_map_pages: None,
            cached_doc: None,
            #[cfg(feature = "incr-glyph-maps")]
            cached_span_ranges: None,
        };
        this.inner.set_should_attach_debug_info(true);
        this
    }
}

impl IncrServer {
    pub(crate) fn update(&mut self, doc: Arc<TypstPagedDocument>) -> PackedDelta {
        // Cache doc for on-demand extraction (hydration)
        self.cached_doc = Some(doc.clone());
        #[cfg(feature = "incr-glyph-maps")]
        {
            // New doc implies new span ranges.
            self.cached_span_ranges = None;
        }

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

    /// Extract glyph positions and local glyph offsets for specific pages.
    /// Used by hydration in incr-glyph-maps builds to fuse glyphMaps from the
    /// same typst2vec traversal as glyphPositions/layoutMap.
    #[cfg(feature = "incr-glyph-maps")]
    pub(crate) fn extract_glyph_positions_and_offsets_for_pages(
        &self,
        pages: &HashSet<u32>,
    ) -> Option<(Vec<PageLayout>, Vec<PageGlyphPositions>, Vec<PageGlyphOffsets>)> {
        let doc = self.cached_doc.as_ref()?;

        let mut pass = IncrTypst2VecPass::default();
        pass.spans.set_should_attach_debug_info(self.attach_debug_info);

        let (layout_map, glyph_positions, glyph_offsets) =
            pass.paged_filtered_with_glyph_offsets(doc, pages);

        Some((layout_map, glyph_positions, glyph_offsets))
    }

    /// Store span ranges for the current cached document.
    #[cfg(feature = "incr-glyph-maps")]
    pub(crate) fn set_cached_span_ranges(
        &mut self,
        ranges: Vec<(String, u32, usize, usize)>,
    ) {
        self.cached_span_ranges = Some(ranges);
    }

    /// Borrow cached span ranges if available.
    #[cfg(feature = "incr-glyph-maps")]
    pub(crate) fn cached_span_ranges(
        &self,
    ) -> Option<&Vec<(String, u32, usize, usize)>> {
        self.cached_span_ranges.as_ref()
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
        #[cfg(feature = "incr-glyph-maps")]
        {
            self.cached_span_ranges = None;
        }
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
