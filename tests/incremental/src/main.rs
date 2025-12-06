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
    let repo_root = project_base.parent().map(|p| p.to_path_buf());
    let w = project_base.join("fonts");
    let font_path = project_base.join("assets/fonts");
    let public_fonts = repo_root
        .map(|r| r.join("public/fonts"))
        .filter(|p| p.exists());
    let verse = TypstSystemUniverse::new(CompileOpts {
        entry: EntryOpts::new_workspace(workspace_dir.into()),
        no_system_fonts: false,
        font_paths: {
            let mut paths = vec![w, font_path];
            if let Some(p) = public_fonts {
                paths.push(p);
            }
            paths
        },
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
    let server_delta = BytesModuleStream::from_slice(&server_delta.bytes).checkout_owned();
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
        let sd = server_delta.bytes.len();
        let server_delta = BytesModuleStream::from_slice(&server_delta.bytes).checkout_owned();
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
    use std::collections::HashSet;
    use std::fs;
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
        let delta = BytesModuleStream::from_slice(&delta.bytes).checkout_owned();
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

    #[test]
    fn layout_map_captures_stroked_and_zero_sized_shapes() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();

        // Two shapes: a stroked rect with known dimensions, and a zero-sized box with stroke.
        let source = r#"
#set page(width: 200pt, height: 200pt, margin: 0pt)
#rect(width: 40pt, height: 10pt, stroke: 2pt)
#box(width: 0pt, height: 0pt, stroke: 1pt)
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        let delta = server.pack_delta(&TypstDocument::Paged(doc));
        let layout_map = delta.layout_map;

        assert_eq!(
            layout_map.len(),
            1,
            "expected single-page layout map for the test document"
        );

        let spans = &layout_map[0].spans;
        assert!(
            !spans.is_empty(),
            "layout map should contain at least the two placed shapes"
        );

        // The stroked rect should produce a bbox at least as large as its intrinsic size.
        let has_stroked_rect = spans
            .iter()
            .any(|bbox| bbox.width >= 40.0 && bbox.height >= 10.0);
        assert!(
            has_stroked_rect,
            "expected a bbox covering the stroked rect (>=40x10pt)"
        );

        // The zero-sized box still carries stroke; bbox should be >0 but not huge.
        let has_zero_sized = spans.iter().any(|bbox| {
            bbox.width > 0.0 && bbox.width <= 6.0 && bbox.height > 0.0 && bbox.height <= 6.0
        });
        assert!(
            has_zero_sized,
            "expected a bbox for the zero-sized stroked box (tiny but non-zero)"
        );
    }

    #[test]
    fn layout_map_uses_per_span_glyph_bounds_for_text() {
        // Use a real template to ensure spans are present and distinct.
        let project_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(3)
            .expect("workspace root")
            .to_path_buf();
        let entry = project_root.join("public/templates/paper.typ");
        assert!(
            entry.exists(),
            "paper template should exist at {:?}",
            entry
        );

        let driver = get_driver(&project_root, &entry);
        let mut server = IncrDocServer::default();
        server.set_should_attach_debug_info(true);

        let paged_doc = driver
            .snapshot_with_entry_content(
                Bytes::from_string(fs::read_to_string(&entry).expect("read template")),
                None,
            )
            .compile()
            .output
            .expect("compile template");

        let typst_doc = TypstDocument::Paged(paged_doc.clone());
        let delta = server.pack_delta(&typst_doc);
        assert_eq!(delta.layout_map.len(), paged_doc.pages.len(), "page count");

        let spans = &delta.layout_map[0].spans;
        assert!(
            spans.len() >= 2,
            "expected multiple spans on first page of paper template, got {}",
            spans.len()
        );

        let unique_x: HashSet<i32> = spans
            .iter()
            .map(|b| (b.x * 1000.0).round() as i32)
            .collect();
        let widths: HashSet<i32> = spans
            .iter()
            .map(|b| (b.width * 1000.0).round() as i32)
            .collect();
        assert!(
            unique_x.len() >= 2 && widths.len() >= 2,
            "per-span bboxes should not collapse to identical X/width; spans: {:?}",
            spans
        );
    }

    #[test]
    fn layout_map_covers_common_templates() {
        let project_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(3)
            .expect("workspace root")
            .to_path_buf();

        let templates: &[(&str, usize)] = &[
            ("public/templates/ieee.typ", 50),
            ("public/templates/paper.typ", 20),
            ("public/templates/thesis.typ", 50),
        ];

        for (rel_path, min_spans) in templates {
            let entry = project_root.join(rel_path);
            assert!(
                entry.exists(),
                "template {} should exist at {:?}",
                rel_path,
                entry
            );

            let driver = get_driver(&project_root, &entry);
            let mut server = IncrDocServer::default();
            server.set_should_attach_debug_info(true);

            let paged_doc = driver
                .snapshot_with_entry_content(
                    Bytes::from_string(fs::read_to_string(&entry).expect("read template")),
                    None,
                )
                .compile()
                .output
                .expect("compile template");

            let page_count = paged_doc.pages.len();
            let typst_doc = TypstDocument::Paged(paged_doc.clone());
            let delta = server.pack_delta(&typst_doc);

            assert_eq!(
                delta.layout_map.len(),
                page_count,
                "layout map pages should match document pages for {}",
                rel_path
            );

            let total_spans: usize = delta.layout_map.iter().map(|p| p.spans.len()).sum();
            assert!(
                total_spans >= *min_spans,
                "expected at least {} spans in layout map for {}, got {}",
                min_spans,
                rel_path,
                total_spans
            );

            for page in &delta.layout_map {
                assert!(
                    !page.spans.is_empty(),
                    "page {} of {} has no span bboxes",
                    page.page,
                    rel_path
                );
                let page_size = paged_doc
                    .pages
                    .get(page.page as usize)
                    .expect("page exists in paged_doc")
                    .frame
                    .size();
                let page_w = page_size.x.to_pt() as f32;
                let page_h = page_size.y.to_pt() as f32;
                for bbox in &page.spans {
                    assert!(
                        bbox.width > 0.0 && bbox.height > 0.0,
                        "zero-sized bbox in {} page {}: {:?}",
                        rel_path,
                        page.page,
                        bbox
                    );
                    // Allow a small tolerance for stroke inflation and glyph ascenders that
                    // protrude slightly outside the nominal page box.
                    let epsilon = 4.0;
                    assert!(
                        bbox.x + bbox.width <= page_w + epsilon,
                        "bbox exceeds page width in {} page {}: {:?} (page_w={})",
                        rel_path,
                        page.page,
                        bbox,
                        page_w
                    );
                    assert!(
                        bbox.y + bbox.height <= page_h + epsilon,
                        "bbox exceeds page height in {} page {}: {:?} (page_h={})",
                        rel_path,
                        page.page,
                        bbox,
                        page_h
                    );
                    assert!(
                        bbox.x >= -epsilon && bbox.y >= -epsilon,
                        "bbox has negative origin in {} page {}: {:?}",
                        rel_path,
                        page.page,
                        bbox
                    );
                }
            }
        }
    }

    #[test]
    fn glyph_positions_align_with_glyph_spans() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();

        let source = r#"
#set page(width: 200pt, height: 100pt, margin: 10pt)
Hello world!
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        let delta = server.pack_delta(&TypstDocument::Paged(doc));
        let glyph_map = delta.glyph_map;

        assert!(
            !glyph_map.is_empty(),
            "glyph_map should contain at least one page"
        );

        let page0 = &glyph_map[0];
        assert!(
            !page0.spans.is_empty(),
            "glyph_map page 0 should contain at least one span run"
        );

        for run in &page0.spans {
            let positions = &run.positions;
            let glyphs = &run.glyph_spans;
            assert!(
                positions.len() >= 2,
                "positions should include start and end edges"
            );
            assert_eq!(
                positions.len(),
                glyphs.len() + 1,
                "positions should be glyph_count + 1 for trailing edge"
            );
            // Positions must be non-decreasing; allow zero-width glyphs but require some advance.
            let mut has_advance = false;
            for window in positions.windows(2) {
                assert!(
                    window[0] <= window[1],
                    "glyph positions must be non-decreasing"
                );
                if window[1] > window[0] {
                    has_advance = true;
                }
            }
            assert!(
                has_advance,
                "expected at least one glyph with positive advance in span {}",
                run.span
            );
        }
    }

}
