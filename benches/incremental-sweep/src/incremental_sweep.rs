use std::sync::{Arc, Mutex};
use std::sync::LazyLock;

use divan::Bencher;
use reflexo_typst::{
    EntryReader, ShadowApiExt, TypstPagedDocument, TypstSystemUniverse, MEMORY_MAIN_ENTRY,
};
use reflexo_typst2vec::incr::IncrDocServer;
use typst::foundations::Bytes;
use reflexo_typst::TypstDocument;
use typst_ts_cli::CompileOnceArgs;

type CompileDriver = LazyLock<Mutex<TypstSystemUniverse>>;

static DRIVER: CompileDriver = LazyLock::new(|| {
    Mutex::new(typst_ts_cli::compile::resolve_universe(CompileOnceArgs {
        workspace: "/".to_owned(),
        entry: "/main.typ".to_owned(),
        ..Default::default()
    }))
});

fn compile(src: &str) -> Arc<TypstPagedDocument> {
    let mut driver = DRIVER.lock().unwrap();
    let e = driver.main_id().unwrap_or_else(|| *MEMORY_MAIN_ENTRY);
    driver
        .with_shadow_file_by_id(e, Bytes::new(src.as_bytes().to_vec()), |this| {
            this.computation().compile().output
        })
        .unwrap()
}

fn make_source(pages: usize) -> String {
    let mut s = String::from(
        "#set page(width: 340pt, height: 140pt, margin: 10pt)\n#set text(12pt)\n\n",
    );

    for i in 0..pages {
        s.push_str(&format!("= Page {}\n\n", i + 1));
        s.push_str("This is filler text to force deterministic page counts.\n\n");
        s.push_str(
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n\n",
        );

        if i + 1 != pages {
            s.push_str("#pagebreak()\n\n");
        }
    }

    s
}

macro_rules! make_docs {
    ($name:ident, $pages:expr) => {
        static $name: LazyLock<(Arc<TypstPagedDocument>, Arc<TypstPagedDocument>)> =
            LazyLock::new(|| {
                let base = make_source($pages);
                let edited = format!("{base}\n\nx");
                (compile(&base), compile(&edited))
            });
    };
}

make_docs!(DOCS_1, 1);
make_docs!(DOCS_5, 5);
make_docs!(DOCS_10, 10);
make_docs!(DOCS_25, 25);
make_docs!(DOCS_50, 50);
make_docs!(DOCS_100, 100);

fn bench_incr(docs: &(Arc<TypstPagedDocument>, Arc<TypstPagedDocument>), bencher: Bencher) {
    let (base_doc, edited_doc) = docs;

    bencher.bench_local(|| {
        let mut server = IncrDocServer::default();
        server.set_should_attach_debug_info(true);

        // Initial baseline delta.
        let _ = server.pack_delta(&TypstDocument::Paged(base_doc.clone()));
        // Incremental delta after a tiny edit.
        let _ = server.pack_delta(&TypstDocument::Paged(edited_doc.clone()));

        // Keep comemo cache bounded between iterations.
        comemo::evict(10);
    });
}

#[divan::bench]
fn incr_sweep_1p(bencher: Bencher) {
    bench_incr(&DOCS_1, bencher);
}

#[divan::bench]
fn incr_sweep_5p(bencher: Bencher) {
    bench_incr(&DOCS_5, bencher);
}

#[divan::bench]
fn incr_sweep_10p(bencher: Bencher) {
    bench_incr(&DOCS_10, bencher);
}

#[divan::bench]
fn incr_sweep_25p(bencher: Bencher) {
    bench_incr(&DOCS_25, bencher);
}

#[divan::bench]
fn incr_sweep_50p(bencher: Bencher) {
    bench_incr(&DOCS_50, bencher);
}

#[divan::bench]
fn incr_sweep_100p(bencher: Bencher) {
    bench_incr(&DOCS_100, bencher);
}

fn main() {
    // Initialize global driver and docs lazily.
    let guard = DRIVER.lock().unwrap();
    drop(guard);
    let _ = &*DOCS_1;

    divan::main();
}
