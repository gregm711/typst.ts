use std::collections::HashSet;
use std::sync::Arc;

use reflexo_typst::{TypstDocument, TypstPagedDocument};
use reflexo_typst2vec::incr::{IncrDocServer, PackedDelta};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct IncrServer {
    inner: IncrDocServer,
    attach_debug_info: bool,
    /// Optional set of page indices to include in glyph map/positions/layout output.
    /// If None, all pages are included. If Some, only listed pages.
    /// This is reset after each incr_compile call via take().
    glyph_map_pages: Option<HashSet<u32>>,
}

impl Default for IncrServer {
    fn default() -> Self {
        let mut this = Self {
            inner: IncrDocServer::default(),
            attach_debug_info: true,
            glyph_map_pages: None,
        };
        this.inner.set_should_attach_debug_info(true);
        this
    }
}

impl IncrServer {
    pub(crate) fn update(&mut self, doc: Arc<TypstPagedDocument>) -> PackedDelta {
        // evicted by compiler
        // comemo::evict(30);

        self.inner.pack_delta(&TypstDocument::Paged(doc))
    }

    /// Take the glyph_map_pages filter, resetting it to None.
    /// Called at the start of incr_compile to get the per-compile filter.
    pub(crate) fn take_glyph_map_pages(&mut self) -> Option<HashSet<u32>> {
        self.glyph_map_pages.take()
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
    }
}
