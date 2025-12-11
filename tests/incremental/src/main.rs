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

    /// Test that numbered list items with mixed styled/plain text get full glyph coverage.
    ///
    /// This tests the fix for synthetic span remapping where plain text after bold
    /// list markers (e.g., "+ *Bold*: Plain text") was not getting glyph positions.
    #[test]
    fn list_item_glyph_coverage_matches_byte_offsets() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();

        // Numbered list with bold headings followed by plain text (mimics the test-editor content)
        let source = r#"
#set page(width: 400pt, height: 200pt, margin: 10pt)

+ *Manual Typesetting*: Physical type composition
+ *Desktop Publishing*: WYSIWYG editors emerge
+ *Web-Based Tools*: Collaborative platforms
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

        // Count total glyph positions across all spans
        let total_positions: usize = page0.spans.iter()
            .map(|run| run.glyph_spans.len())
            .sum();

        // The source has roughly:
        // - 3 list markers (1., 2., 3.) - synthetic but should be mapped
        // - "*Manual Typesetting*" (18 chars) + ": Physical type composition" (28 chars)
        // - "*Desktop Publishing*" (18 chars) + ": WYSIWYG editors emerge" (24 chars)
        // - "*Web-Based Tools*" (15 chars) + ": Collaborative platforms" (26 chars)
        // Total visible chars ~130+

        // We expect at least 100 glyph positions (allowing for ligatures etc)
        assert!(
            total_positions >= 100,
            "expected at least 100 glyph positions for list content, got {}. \
             This indicates synthetic spans or plain text after bold may be missing coverage.",
            total_positions
        );

        // Verify each span has reasonable coverage (positions should be close to glyph count)
        for run in &page0.spans {
            let glyph_count = run.glyph_spans.len();
            let position_count = run.positions.len();

            // positions should be glyph_count + 1 (for trailing edge)
            assert_eq!(
                position_count,
                glyph_count + 1,
                "span {} has {} glyphs but {} positions (expected {})",
                run.span,
                glyph_count,
                position_count,
                glyph_count + 1
            );
        }

        // Check that we have multiple spans (not everything collapsed into one)
        assert!(
            page0.spans.len() >= 3,
            "expected at least 3 spans for list with 3 items, got {}",
            page0.spans.len()
        );
    }

    /// CRITICAL TEST: Verify span count alignment between typst2vec (delta.glyph_map)
    /// and what the main-thread glyph map collector would produce.
    ///
    /// BUG REPRODUCTION: During hydration of pages 2+, the console shows:
    ///   glyphPositions=[{"page":1,"spanCount":21}]
    ///   glyphMaps=[{"page":1,"spanCount":24}]
    ///
    /// This 3-span mismatch causes heading clicks to fail because:
    /// - glyphMaps (from collect_glyph_maps in lib.rs) has the heading spans
    /// - glyphPositions (from delta.glyph_map via typst2vec) is missing them
    /// - The filtering in incr_compile keeps only spans in BOTH
    /// - Result: headings have byte offsets but no X/Y positions
    ///
    /// Root cause: typst2vec.rs and lib.rs traverse the frame differently,
    /// or one filters out certain spans that the other includes.
    #[test]
    fn glyph_map_span_count_matches_glyph_positions_span_count() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();
        server.set_should_attach_debug_info(true);

        // Multi-page document with headings (the bug case)
        let source = r#"
#set page(width: 400pt, height: 300pt, margin: 20pt)

= Chapter 1: Introduction

This is the first paragraph. It contains regular text.

== Section 1.1

More content here with some text to fill the page.

#pagebreak()

= Chapter 2: Methods

This is page 2 of the document with a chapter heading.

== Section 2.1

Additional content for the second page.
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        let page_count = doc.pages.len();
        assert!(page_count >= 2, "test document should have at least 2 pages");

        let delta = server.pack_delta(&TypstDocument::Paged(doc));

        // delta.glyph_map comes from typst2vec (the "glyphPositions" in JS)
        let glyph_positions = &delta.glyph_map;

        // delta.layout_map also comes from typst2vec
        let layout_map = &delta.layout_map;

        eprintln!("\n=== Span Count Analysis ===");
        for page_gp in glyph_positions {
            let gp_span_count = page_gp.spans.len();
            eprintln!("Page {}: glyphPositions has {} spans", page_gp.page, gp_span_count);

            // Find corresponding layout map page
            if let Some(page_lm) = layout_map.iter().find(|lm| lm.page == page_gp.page) {
                let lm_span_count = page_lm.spans.len();
                eprintln!("Page {}: layoutMap has {} spans", page_lm.page, lm_span_count);
            }
        }

        // Collect unique span IDs from glyphPositions
        let gp_span_ids: std::collections::HashSet<u64> = glyph_positions
            .iter()
            .flat_map(|page| page.spans.iter().map(|s| s.span))
            .collect();

        // Collect unique span IDs from layoutMap
        let lm_span_ids: std::collections::HashSet<u64> = layout_map
            .iter()
            .flat_map(|page| page.spans.iter().map(|s| s.span))
            .collect();

        eprintln!("\nTotal unique spans:");
        eprintln!("  glyphPositions: {} spans", gp_span_ids.len());
        eprintln!("  layoutMap: {} spans", lm_span_ids.len());

        // Find spans in layoutMap but not in glyphPositions (potential headings)
        let missing_from_gp: Vec<_> = lm_span_ids.difference(&gp_span_ids).collect();
        if !missing_from_gp.is_empty() {
            eprintln!("\n!! MISSING from glyphPositions (but in layoutMap): {:?}", missing_from_gp);
        }

        // Find spans in glyphPositions but not in layoutMap
        let missing_from_lm: Vec<_> = gp_span_ids.difference(&lm_span_ids).collect();
        if !missing_from_lm.is_empty() {
            eprintln!("!! MISSING from layoutMap (but in glyphPositions): {:?}", missing_from_lm);
        }

        // KEY ASSERTION: glyphPositions and layoutMap should have the same span coverage
        // If this fails, it indicates the bug where one code path misses spans
        assert!(
            missing_from_gp.is_empty(),
            "SPAN MISMATCH BUG: {} spans in layoutMap missing from glyphPositions. \
             These spans will have bounding boxes but no glyph positions, \
             causing clicks to fail. Missing: {:?}",
            missing_from_gp.len(),
            missing_from_gp
        );

        // Each page should have consistent span counts between the two sources
        for page_gp in glyph_positions {
            if let Some(page_lm) = layout_map.iter().find(|lm| lm.page == page_gp.page) {
                let gp_spans: std::collections::HashSet<_> =
                    page_gp.spans.iter().map(|s| s.span).collect();
                let lm_spans: std::collections::HashSet<_> =
                    page_lm.spans.iter().map(|s| s.span).collect();

                // Find per-page differences
                let page_missing: Vec<_> = lm_spans.difference(&gp_spans).collect();
                if !page_missing.is_empty() {
                    eprintln!(
                        "Page {}: {} spans in layoutMap missing from glyphPositions: {:?}",
                        page_gp.page, page_missing.len(), page_missing
                    );
                }

                assert!(
                    page_missing.is_empty(),
                    "Page {}: layoutMap has {} spans not in glyphPositions. \
                     This causes click failures on these elements.",
                    page_gp.page,
                    page_missing.len()
                );
            }
        }

        eprintln!("\n=== PASS: Span counts match ===\n");
    }

    /// Test that plain text following bold/emphasized text in list items gets glyph positions.
    ///
    /// The bug: glyphMaps correctly assigns byte offsets, but glyphPositions uses
    /// different span IDs causing a mismatch after the coverage filter.
    #[test]
    fn plain_text_after_emphasis_has_glyph_positions() {
        let driver = make_driver();
        let mut server = IncrDocServer::default();

        // Simple case: bold followed by plain text
        let source = r#"
#set page(width: 300pt, height: 100pt, margin: 10pt)

*Bold text*: followed by plain text here
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        let delta = server.pack_delta(&TypstDocument::Paged(doc));
        let glyph_map = delta.glyph_map;

        assert!(!glyph_map.is_empty(), "should have glyph map");
        let page0 = &glyph_map[0];

        // The text "followed by plain text here" has ~28 characters
        // We need to verify it has glyph positions, not just 2 positions (colon only)

        // Find spans that cover positions after x=100pt (roughly where the colon would be)
        let spans_with_good_coverage: Vec<_> = page0.spans.iter()
            .filter(|run| {
                // Check if span has more than just a couple glyphs
                run.glyph_spans.len() > 5
            })
            .collect();

        // Should have at least 2 spans with good coverage:
        // 1. The bold text "*Bold text*"
        // 2. The plain text ": followed by plain text here"
        assert!(
            spans_with_good_coverage.len() >= 2,
            "expected at least 2 spans with >5 glyphs (bold and plain text), got {}. \
             Spans: {:?}",
            spans_with_good_coverage.len(),
            page0.spans.iter().map(|r| (r.span, r.glyph_spans.len())).collect::<Vec<_>>()
        );

        // Verify the total glyph count covers most of the text
        let total_glyphs: usize = page0.spans.iter()
            .map(|r| r.glyph_spans.len())
            .sum();

        // "Bold text" (9) + "followed by plain text here" (28) + colon/space = ~40 chars minimum
        assert!(
            total_glyphs >= 35,
            "expected at least 35 total glyphs, got {}. Plain text may be missing.",
            total_glyphs
        );
    }

    /// CARMACK PARITY TEST: Compare span IDs from lib.rs-style traversal vs typst2vec.
    ///
    /// This test mimics `collect_glyph_maps` traversal logic and compares the resulting
    /// span IDs with what typst2vec produces in `delta.glyph_map`.
    ///
    /// If this test fails, it means the two code paths diverge, which causes:
    /// - Spans to be filtered out in `incr_compile`
    /// - Clicks to fail on hydrated pages
    #[test]
    fn span_parity_lib_rs_vs_typst2vec() {
        use typst::layout::FrameItem;

        let driver = make_driver();
        let mut server = IncrDocServer::default();
        server.set_should_attach_debug_info(true);

        // Multi-page document with headings and paragraphs
        let source = r#"
#set page(width: 400pt, height: 300pt, margin: 20pt)

= Chapter 1: Introduction

This is the first paragraph. It contains regular text that should be clickable.

== Section 1.1

More content here with some text to fill the page.

#pagebreak()

= Chapter 2: Methods

This is page 2 of the document with a chapter heading.

== Section 2.1

Additional content for the second page.
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        // Get typst2vec's span IDs
        let delta = server.pack_delta(&TypstDocument::Paged(doc.clone()));
        let typst2vec_span_ids: HashSet<u64> = delta.glyph_map
            .iter()
            .flat_map(|page| page.spans.iter().map(|s| s.span))
            .collect();

        // Extract span IDs the way lib.rs collect_glyph_maps does:
        // Traverse frames, find FrameItem::Text, extract span IDs from glyphs
        fn collect_spans_from_frame(frame: &typst::layout::Frame, result: &mut HashSet<u64>) {
            for (_pos, item) in frame.items() {
                match item {
                    FrameItem::Group(group) => {
                        collect_spans_from_frame(&group.frame, result);
                    }
                    FrameItem::Text(text) => {
                        // Find anchor span (first non-detached glyph)
                        let anchor_id: u64 = text.glyphs.iter()
                            .find(|g| !g.span.0.is_detached())
                            .map(|g| g.span.0.into_raw().get())
                            .unwrap_or(0);

                        // Skip purely synthetic items (matches lib.rs behavior)
                        if anchor_id == 0 {
                            continue;
                        }

                        // Collect span IDs from all glyphs (using anchor for detached)
                        for glyph in text.glyphs.iter() {
                            let span_id = if glyph.span.0.is_detached() {
                                anchor_id
                            } else {
                                glyph.span.0.into_raw().get()
                            };
                            if span_id != 0 {
                                result.insert(span_id);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut lib_rs_span_ids: HashSet<u64> = HashSet::new();
        for page in &doc.pages {
            collect_spans_from_frame(&page.frame, &mut lib_rs_span_ids);
        }

        eprintln!("\n=== SPAN PARITY TEST ===");
        eprintln!("lib.rs-style traversal: {} unique spans", lib_rs_span_ids.len());
        eprintln!("typst2vec glyph_map: {} unique spans", typst2vec_span_ids.len());

        // Find divergences
        let in_lib_not_typst2vec: Vec<_> = lib_rs_span_ids.difference(&typst2vec_span_ids)
            .map(|id| format!("{:x}", id))
            .collect();
        let in_typst2vec_not_lib: Vec<_> = typst2vec_span_ids.difference(&lib_rs_span_ids)
            .map(|id| format!("{:x}", id))
            .collect();

        if !in_lib_not_typst2vec.is_empty() {
            eprintln!("\n!! IN lib.rs BUT NOT typst2vec ({} spans):", in_lib_not_typst2vec.len());
            for (i, span_id) in in_lib_not_typst2vec.iter().take(10).enumerate() {
                eprintln!("  {}. {}", i + 1, span_id);
            }
        }

        if !in_typst2vec_not_lib.is_empty() {
            eprintln!("\n!! IN typst2vec BUT NOT lib.rs ({} spans):", in_typst2vec_not_lib.len());
            for (i, span_id) in in_typst2vec_not_lib.iter().take(10).enumerate() {
                eprintln!("  {}. {}", i + 1, span_id);
            }
        }

        // PARITY ASSERTION: Both should produce the same spans
        assert!(
            in_lib_not_typst2vec.is_empty() && in_typst2vec_not_lib.is_empty(),
            "SPAN PARITY VIOLATION:\n\
             - {} spans in lib.rs but NOT typst2vec: {:?}\n\
             - {} spans in typst2vec but NOT lib.rs: {:?}\n\
             This divergence causes click failures on hydrated pages.",
            in_lib_not_typst2vec.len(), in_lib_not_typst2vec.iter().take(5).collect::<Vec<_>>(),
            in_typst2vec_not_lib.len(), in_typst2vec_not_lib.iter().take(5).collect::<Vec<_>>()
        );

        eprintln!("\n=== SPAN PARITY PASS ===\n");
    }

    /// Test per-page span parity to catch divergence in page-filtered scenarios (hydration).
    ///
    /// The bug manifests when:
    /// - Full compile: page 0 cached
    /// - Scroll to page 2: page 2 compiled with filter
    /// - Span counts differ between cached page 0 data and newly compiled page 2 data
    #[test]
    fn per_page_span_parity() {
        use typst::layout::FrameItem;

        let driver = make_driver();
        let mut server = IncrDocServer::default();
        server.set_should_attach_debug_info(true);

        // Multi-page document
        let source = r#"
#set page(width: 400pt, height: 300pt, margin: 20pt)

= Page 0 Heading

Paragraph on page zero with some content.

#pagebreak()

= Page 1 Heading

Another paragraph on page one.

#pagebreak()

= Page 2 Heading

Third paragraph on page two.
"#;

        let doc = driver
            .snapshot_with_entry_content(Bytes::from_string(source.to_owned()), None)
            .compile()
            .output
            .expect("compile document");

        assert!(doc.pages.len() >= 3, "Need at least 3 pages");

        // Get typst2vec's span IDs per page
        let delta = server.pack_delta(&TypstDocument::Paged(doc.clone()));

        // Helper to collect spans from a single frame
        fn collect_spans_from_frame(frame: &typst::layout::Frame, result: &mut HashSet<u64>) {
            for (_pos, item) in frame.items() {
                match item {
                    FrameItem::Group(group) => {
                        collect_spans_from_frame(&group.frame, result);
                    }
                    FrameItem::Text(text) => {
                        let anchor_id: u64 = text.glyphs.iter()
                            .find(|g| !g.span.0.is_detached())
                            .map(|g| g.span.0.into_raw().get())
                            .unwrap_or(0);

                        if anchor_id == 0 {
                            continue;
                        }

                        for glyph in text.glyphs.iter() {
                            let span_id = if glyph.span.0.is_detached() {
                                anchor_id
                            } else {
                                glyph.span.0.into_raw().get()
                            };
                            if span_id != 0 {
                                result.insert(span_id);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        eprintln!("\n=== PER-PAGE SPAN PARITY TEST ===");

        for (page_idx, page) in doc.pages.iter().enumerate() {
            // lib.rs-style spans for this page
            let mut lib_rs_spans: HashSet<u64> = HashSet::new();
            collect_spans_from_frame(&page.frame, &mut lib_rs_spans);

            // typst2vec spans for this page
            let typst2vec_page = delta.glyph_map.iter().find(|p| p.page == page_idx as u32);
            let typst2vec_spans: HashSet<u64> = typst2vec_page
                .map(|p| p.spans.iter().map(|s| s.span).collect())
                .unwrap_or_default();

            let in_lib_not_t2v: Vec<_> = lib_rs_spans.difference(&typst2vec_spans)
                .map(|id| format!("{:x}", id))
                .collect();
            let in_t2v_not_lib: Vec<_> = typst2vec_spans.difference(&lib_rs_spans)
                .map(|id| format!("{:x}", id))
                .collect();

            eprintln!(
                "Page {}: lib.rs={} spans, typst2vec={} spans, diff=[+{}, -{}]",
                page_idx,
                lib_rs_spans.len(),
                typst2vec_spans.len(),
                in_lib_not_t2v.len(),
                in_t2v_not_lib.len()
            );

            if !in_lib_not_t2v.is_empty() {
                eprintln!("  IN lib.rs NOT typst2vec: {:?}", in_lib_not_t2v.iter().take(5).collect::<Vec<_>>());
            }
            if !in_t2v_not_lib.is_empty() {
                eprintln!("  IN typst2vec NOT lib.rs: {:?}", in_t2v_not_lib.iter().take(5).collect::<Vec<_>>());
            }

            assert!(
                in_lib_not_t2v.is_empty() && in_t2v_not_lib.is_empty(),
                "Page {} span parity violation", page_idx
            );
        }

        eprintln!("\n=== PER-PAGE SPAN PARITY PASS ===\n");
    }

}
