use std::path::Path;

use reflexo_typst::config::{entry::EntryOpts, CompileOpts};
use reflexo_typst::vector::{
    ir::{Abs, Point, Rect},
    stream::BytesModuleStream,
};
use reflexo_typst::{Bytes, TypstDocument, TypstSystemUniverse};
use reflexo_typst2vec::incr::{IncrDocClient, IncrDocServer};
use reflexo_vec2svg::IncrSvgDocClient;

fn get_driver(workspace_dir: &Path, entry_file_path: &Path) -> TypstSystemUniverse {
    let project_base = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let w = project_base.join("fonts");
    let font_path = project_base.join("assets/fonts");
    let verse = TypstSystemUniverse::new(CompileOpts {
        entry: EntryOpts::new_workspace(workspace_dir.into()),
        no_system_fonts: true,
        font_paths: vec![w, font_path],
        ..CompileOpts::default()
    })
    .unwrap();

    verse.with_entry_file(entry_file_path.to_owned())
}

pub fn test_compiler(workspace_dir: &Path, entry_file_path: &Path) {
    let driver = get_driver(workspace_dir, entry_file_path);
    let mut content = { std::fs::read_to_string(entry_file_path).expect("Could not read file") };

    let mut incr_server = IncrDocServer::default();
    let mut incr_client = IncrDocClient::default();
    let mut incr_svg_client = IncrSvgDocClient::default();

    let window = Rect {
        lo: Point::new(Abs::from(0.), Abs::from(0.)),
        hi: Point::new(Abs::from(1e33), Abs::from(1e33)),
    };
    let _ = incr_svg_client.render_in_window(&mut incr_client, window);

    let mut diff = vec![];

    // checkout the entry file

    let doc = driver
        .snapshot_with_entry_content(Bytes::from_string(content.clone()), None)
        .compile()
        .output
        .unwrap();
    let server_delta = incr_server.pack_delta(&TypstDocument::Paged(doc));
    let server_delta = BytesModuleStream::from_slice(&server_delta).checkout_owned();
    incr_client.merge_delta(server_delta);
    let _ = incr_svg_client.render_in_window(&mut incr_client, window);

    for i in 0..20 {
        eprintln!("Iteration {i}");

        // content = content.replace("@netwok2020", "@netwok2020 x");
        content += "\n\nx";

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(content.clone()), None)
            .compile()
            .output
            .unwrap();

        let server_delta = incr_server.pack_delta(&TypstDocument::Paged(doc));
        let sd = server_delta.len();
        let server_delta = BytesModuleStream::from_slice(&server_delta).checkout_owned();
        incr_client.merge_delta(server_delta);
        incr_client.set_layout(incr_client.doc.layouts[0].unwrap_single());
        let cd = incr_svg_client.render_in_window(&mut incr_client, window);
        // std::fs::write(format!("{}.svg", i), cd.clone()).unwrap();
        diff.push((sd, cd.len()));

        comemo::evict(10);
    }

    eprintln!("diff: {diff:?}");
}

pub fn main() {
    // #[cfg(feature = "ieee")]
    let workspace_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../");
    // #[cfg(feature = "ieee")]
    let entry_file_path = workspace_dir.join("fuzzers/corpora/typst-templates/ieee/main.typ");

    #[cfg(feature = "pku-thesis")]
    let workspace_dir =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../../ts/pkuthss-typst/");
    #[cfg(feature = "pku-thesis")]
    let entry_file_path = workspace_dir.join(r#"thesis.typ"#);

    // let workspace_dir =
    //     std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../../typst/
    // masterproef/"); let entry_file_path =
    // workspace_dir.join(r#"masterproef/main.typ"#);

    for i in 0..10 {
        eprintln!("Over Iteration {i}");
        test_compiler(&workspace_dir, &entry_file_path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reflexo::hash::Fingerprint;
    use reflexo_typst2vec::stream::BytesModuleStream;
    use std::path::Path;

    /// Compile source through the incremental server/client pipeline and return page fingerprints.
    fn page_fingerprints(
        server: &mut IncrDocServer,
        client: &mut IncrDocClient,
        driver: &TypstSystemUniverse,
        source: &str,
    ) -> Vec<Fingerprint> {
        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        let delta = server.pack_delta(&TypstDocument::Paged(doc));
        let delta = BytesModuleStream::from_slice(&delta).checkout_owned();
        client.merge_delta(delta);
        client.set_layout(client.doc.layouts[0].unwrap_single());

        let layout = client.layout.as_ref().expect("layout present after merge");
        let pages = layout
            .pages(client.module())
            .expect("page view available after layout");

        pages.pages().iter().map(|p| p.content).collect()
    }

    fn make_driver() -> TypstSystemUniverse {
        let project_base = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let workspace_dir = project_base.clone();
        let entry_file_path = project_base.join("fuzzers/corpora/typst-templates/ieee/main.typ");
        get_driver(&workspace_dir, &entry_file_path)
    }

    fn base_and_edited() -> (&'static str, &'static str) {
        let base_src = r#"
#set page(width: 340pt, height: 140pt)
#set text(12pt)

= Incremental Fingerprint Test
Page one base content.
#pagebreak()
Page two base content.
#pagebreak()
Page three base content.
#pagebreak()
Page four base content.
"#;

        let edited_src = r#"
#set page(width: 340pt, height: 140pt)
#set text(12pt)

= Incremental Fingerprint Test (edited)
Page one edited content with an extra sentence.
#pagebreak()
Page two base content.
#pagebreak()
Page three base content.
#pagebreak()
Page four base content.
"#;

        (base_src, edited_src)
    }

    #[test]
    fn layout_version_bumps_even_if_page_fingerprints_stable() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();
        let mut client = IncrDocClient::default();
        let (base_src, edited_src) = base_and_edited();

        let initial_version = client.layout_version;
        let before = page_fingerprints(&mut server, &mut client, &driver, base_src);
        let mid_version = client.layout_version;
        assert!(
            before.len() >= 2,
            "expected multi-page doc from base content, got {} page(s)",
            before.len()
        );

        let after = page_fingerprints(&mut server, &mut client, &driver, edited_src);
        let final_version = client.layout_version;
        assert_eq!(
            before.len(),
            after.len(),
            "page count changed after small edit; adjust fixture to keep counts stable"
        );

        // Layout version should bump on each merge, even if fingerprints are identical.
        assert!(
            mid_version > initial_version && final_version > mid_version,
            "layout_version did not increment across merges (initial={}, mid={}, final={})",
            initial_version,
            mid_version,
            final_version
        );

        let fingerprints_changed = before
            .iter()
            .zip(after.iter())
            .any(|(a, b)| a != b);

        assert!(
            fingerprints_changed || final_version != mid_version,
            "Page fingerprints stayed identical and layout_version did not bump; renderer would skip repaint"
        );
    }

}
