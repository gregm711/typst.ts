use std::sync::Arc;

use reflexo_typst::{TypstDocument, TypstPagedDocument};
use reflexo_typst2vec::incr::{IncrDocServer, PackedDelta};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct IncrServer {
    inner: IncrDocServer,
    attach_debug_info: bool,
}

impl Default for IncrServer {
    fn default() -> Self {
        let mut this = Self {
            inner: IncrDocServer::default(),
            attach_debug_info: true,
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
}

#[wasm_bindgen]
impl IncrServer {
    pub fn set_attach_debug_info(&mut self, attach: bool) {
        self.attach_debug_info = attach;
        self.inner.set_should_attach_debug_info(attach);
    }

    pub fn current(&mut self) -> Option<Vec<u8>> {
        self.inner.pack_current()
    }

    pub fn reset(&mut self) {
        self.inner = IncrDocServer::default();
        self.inner
            .set_should_attach_debug_info(self.attach_debug_info);
    }
}
