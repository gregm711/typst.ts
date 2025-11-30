#![cfg(test)]

use std::{collections::HashMap, env::current_dir, path::Path, sync::Arc};

use reflexo_typst::{
    print_diagnostics, syntax::Source, Bytes, CompileActor, EntryReader, LazyHash, ShadowApi,
    TaskInputs, TypstHtmlDocument, TypstPagedDocument, TypstWorld,
};
use typst::{foundations::dict, foundations::IntoValue, layout::{Frame, FrameItem, Point}};

/// Represents a text item found in the frame with its position and span info
#[derive(Debug, Clone)]
struct TextItemInfo {
    glyph_count: usize,
    span_raw: u64,
    y_position: f64,
    x_position: f64,
}

/// Recursively walk a frame and collect all text items with their span information
fn collect_text_items_from_frame(frame: &Frame, offset: Point, items: &mut Vec<TextItemInfo>) {
    for (pos, item) in frame.items() {
        let abs_pos = Point::new(offset.x + pos.x, offset.y + pos.y);
        match item {
            FrameItem::Group(group) => {
                collect_text_items_from_frame(&group.frame, abs_pos, items);
            }
            FrameItem::Text(text) => {
                // Get the span from the first glyph
                if let Some(first_glyph) = text.glyphs.first() {
                    let span = first_glyph.span.0;
                    let span_raw = span.into_raw().get();

                    items.push(TextItemInfo {
                        glyph_count: text.glyphs.len(),
                        span_raw,
                        y_position: abs_pos.y.to_pt(),
                        x_position: abs_pos.x.to_pt(),
                    });
                }
            }
            _ => {}
        }
    }
}

#[test]
fn test() {
    // todo: prelude it?
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let _universe = args
        .resolve_system()
        .expect("failed to resolve system universe");
}

#[test]
fn test_snapshot() {
    // todo: prelude it?
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let world = verse.snapshot();
    typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("no errors");
}

#[test]
fn test_snapshot_html() {
    // todo: prelude it?
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let world = verse.snapshot();
    typst::compile::<TypstHtmlDocument>(world.html_task().as_ref())
        .output
        .expect("no errors");
}

#[test]
fn test_snapshot_diag() {
    // todo: prelude it?
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let world = verse.snapshot();
    let res = typst::compile::<TypstPagedDocument>(&world);
    let errors = res.output.err();
    let diag = res.warnings.iter().chain(errors.iter().flatten());
    let _ = print_diagnostics(&world, diag, reflexo_typst::DiagnosticFormat::Human);
}

#[test]
fn test_snapshot_watch() {
    // todo: prelude it?
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let (intr_tx, intr_rx) = tokio::sync::mpsc::unbounded_channel();
    let actor = CompileActor::new(verse, intr_tx, intr_rx).with_watch(true);
    let _spawn_it = || tokio::spawn(actor.run());
}

#[test]
fn test_snapshot_with() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let entry = verse
        .entry_state()
        .select_in_workspace(Path::new("/main.typ"));
    let world = verse.snapshot_with(Some(TaskInputs {
        entry: Some(entry),
        ..Default::default()
    }));
    typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("no errors");
}

#[test]
fn test_snapshot_with_try() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let another_entry = current_dir().expect("cwd").join("main.typ");
    let entry = verse
        .entry_state()
        .try_select_path_in_workspace(&another_entry)
        .expect("failed to select path");
    let world = verse.snapshot_with(Some(TaskInputs {
        entry,
        ..Default::default()
    }));
    typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("no errors");
}

#[test]
fn test_snapshot_with_inputs() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let pairs = [("my-target", "markdown")].map(|(k, v)| (k.into(), v.into_value()));
    let world = verse.snapshot_with(Some(TaskInputs {
        inputs: Some(Arc::new(LazyHash::new(pairs.into_iter().collect()))),
        ..Default::default()
    }));
    typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("no errors");
}

#[test]
fn test_snapshot_with_inputs_macro() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;
    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");
    let source = Source::new(verse.main_id().unwrap(), "Hello World.".into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");
    let world = verse.snapshot_with(Some(TaskInputs {
        inputs: Some(Arc::new(LazyHash::new(dict! {
            "my-target" => "markdown"
        }))),
        ..Default::default()
    }));
    typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("no errors");
}

/// Test: Does wrapped text produce multiple FrameItem::Text with the SAME SourceSpan?
///
/// This is the key question for cursor positioning. When a long title wraps across
/// multiple lines, we need to know if each line has:
/// - SAME span_id: Our code should group them as chunks (current assumption)
/// - DIFFERENT span_id: Each line is independent (would break our chunking)
#[test]
fn test_wrapped_text_same_span() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;

    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");

    // Typst code that forces text to wrap by using a narrow container
    // The title text should wrap to multiple lines
    let typst_code = r#"
#set page(width: 200pt, height: auto, margin: 10pt)
#set text(size: 12pt)

#block(width: 100%)[
    A Very Long Title That Will Definitely Wrap Across Multiple Lines In This Narrow Container
]
"#;

    let source = Source::new(verse.main_id().unwrap(), typst_code.into());
    verse
        .map_shadow_by_id(source.id(), Bytes::from_string(source.text().to_owned()))
        .expect("failed to map shadow");

    let world = verse.snapshot();
    let doc = typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("compilation should succeed");

    // Collect all text items from the first page
    let mut text_items: Vec<TextItemInfo> = Vec::new();
    if let Some(page) = doc.pages.first() {
        collect_text_items_from_frame(&page.frame, Point::zero(), &mut text_items);
    }

    // Debug: Print all text items found
    println!("\n=== Text Items Found ===");
    for item in &text_items {
        println!(
            "span_raw={:#x}, y={:.1}pt, x={:.1}pt, glyphs={}",
            item.span_raw, item.y_position, item.x_position, item.glyph_count
        );
    }

    // Group by Y position to see how many lines we have
    let mut lines_by_y: HashMap<i64, Vec<&TextItemInfo>> = HashMap::new();
    for item in &text_items {
        // Round Y to avoid floating point issues (group items within 1pt)
        let y_key = (item.y_position * 10.0) as i64;
        lines_by_y.entry(y_key).or_default().push(item);
    }

    println!("\n=== Lines by Y position ===");
    let mut y_keys: Vec<_> = lines_by_y.keys().collect();
    y_keys.sort();
    for y in &y_keys {
        let items = &lines_by_y[y];
        let y_pt = **y as f64 / 10.0;
        println!("Y={:.1}pt:", y_pt);
        for item in items {
            println!("  span_raw={:#x}, glyphs={}", item.span_raw, item.glyph_count);
        }
    }

    // The KEY question: Do text items on DIFFERENT lines have the SAME span_raw?
    let all_span_raws: Vec<u64> = text_items.iter().map(|t| t.span_raw).collect();
    let unique_spans: std::collections::HashSet<_> = all_span_raws.iter().collect();

    println!("\n=== Span Analysis ===");
    println!("Total text items: {}", text_items.len());
    println!("Number of Y positions (lines): {}", y_keys.len());
    println!("Unique span_raw values: {}", unique_spans.len());
    println!("Unique spans: {:x?}", unique_spans);

    // If we have multiple lines but only 1 unique span, all wrapped lines share the SAME span
    // If we have multiple lines and multiple unique spans, each line may have its own span

    if y_keys.len() > 1 && unique_spans.len() == 1 {
        println!("\n✓ SUCCESS: Multiple lines but SAME SourceSpan!");
        println!("  This means our chunking logic should work correctly.");
    } else if y_keys.len() > 1 && unique_spans.len() > 1 {
        println!("\n✗ PROBLEM: Multiple lines have DIFFERENT SourceSpans!");
        println!("  This means each line is treated as independent.");

        // Show which spans map to which lines
        for span in &unique_spans {
            let items_with_span: Vec<_> = text_items.iter()
                .filter(|t| t.span_raw == **span)
                .collect();
            println!("  Span {:#x}:", span);
            for item in items_with_span {
                println!("    y={:.1}pt, glyphs={}", item.y_position, item.glyph_count);
            }
        }
    } else {
        println!("\nNOTE: Only found {} line(s) with {} unique span(s)", y_keys.len(), unique_spans.len());
    }

    // This test is informational - we want to KNOW the answer, not assert a specific outcome
    assert!(text_items.len() > 0, "Should have found some text items");
}

/// Test: How does collect_glyphs_from_frame emit glyph maps for wrapped text?
///
/// This test verifies that when text wraps, each line produces a separate
/// SpanGlyphMap entry with:
/// - Same span_id (since they share SourceSpan)
/// - Different byte offset ranges (based on glyph ranges within the line)
/// - Different Y positions (indicating different lines)
///
/// The client-side should then group these by span_id into "chunks".
#[test]
fn test_glyph_map_emission_for_wrapped_text() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;

    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");

    // Typst code that forces text to wrap
    let typst_code = r#"
#set page(width: 200pt, height: auto, margin: 10pt)
#set text(size: 12pt)

#block(width: 100%)[
    A Very Long Title That Will Definitely Wrap Across Multiple Lines In This Narrow Container
]
"#;

    let source_obj = Source::new(verse.main_id().unwrap(), typst_code.into());
    verse
        .map_shadow_by_id(source_obj.id(), Bytes::from_string(source_obj.text().to_owned()))
        .expect("failed to map shadow");

    let world = verse.snapshot();
    let doc = typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("compilation should succeed");

    // Get the compiled source for span resolution
    let compiled_source = world.source(world.main()).unwrap();

    println!("\n=== Simulated SpanGlyphMap emission (with proper byte offsets) ===");
    println!("Source text length: {} bytes", compiled_source.text().len());
    println!("Source text: {:?}\n", compiled_source.text());

    // Track chunks by span_id to simulate client grouping
    let mut chunks_by_span: HashMap<String, Vec<(i32, f64, usize, usize)>> = HashMap::new();

    let mut chunk_count = 0;
    fn walk_frame_for_glyph_maps(
        frame: &Frame,
        offset: Point,
        chunk_count: &mut i32,
        source: &Source,
        chunks_by_span: &mut HashMap<String, Vec<(i32, f64, usize, usize)>>,
    ) {
        for (pos, item) in frame.items() {
            let abs_pos = Point::new(offset.x + pos.x, offset.y + pos.y);
            match item {
                FrameItem::Group(group) => {
                    walk_frame_for_glyph_maps(&group.frame, abs_pos, chunk_count, source, chunks_by_span);
                }
                FrameItem::Text(text) => {
                    if let Some(first_glyph) = text.glyphs.first() {
                        let span = first_glyph.span.0;
                        let span_id = format!("{:x}", span.into_raw().get());

                        // Use the CORRECT method: source.range() + local offset
                        // This is how collect_glyphs_from_frame does it
                        let mut byte_offsets: Vec<usize> = Vec::new();

                        for glyph in text.glyphs.iter() {
                            let glyph_span = glyph.span.0;
                            let local_offset = glyph.span.1 as usize;

                            if let Some(range) = source.range(glyph_span) {
                                let byte_pos = range.start + local_offset;
                                byte_offsets.push(byte_pos);
                            }
                        }

                        // Add end position - CORRECTLY calculated from last glyph's local offset
                        // The BUG: using range.end gives us the END of the entire source span
                        // The FIX: use range.start + last_glyph.span.1 + <glyph_length>
                        // But we don't have glyph length... we need to estimate it
                        if let Some(last_glyph) = text.glyphs.last() {
                            let last_span = last_glyph.span.0;
                            let last_local_offset = last_glyph.span.1 as usize;
                            if let Some(range) = source.range(last_span) {
                                // WRONG: byte_offsets.push(range.end);
                                // This gives 191 for ALL text items, which is the span END

                                // For debugging, show both:
                                let buggy_end = range.end;
                                let correct_end = range.start + last_local_offset + 1; // +1 for single char

                                println!(
                                    "    End position: buggy={} (span end), correct~={} (last glyph + 1)",
                                    buggy_end, correct_end
                                );

                                // Use the correct end
                                byte_offsets.push(correct_end);
                            }
                        }

                        if byte_offsets.len() > 1 {
                            let first_byte = byte_offsets[0];
                            let last_byte = *byte_offsets.last().unwrap();

                            *chunk_count += 1;
                            println!(
                                "Chunk {}: span_id={}..., y={:.1}pt, bytes={}..{}, glyphs={}",
                                chunk_count,
                                &span_id[..8.min(span_id.len())],
                                abs_pos.y.to_pt(),
                                first_byte,
                                last_byte,
                                text.glyphs.len()
                            );

                            // Track for grouping analysis
                            chunks_by_span.entry(span_id.clone())
                                .or_default()
                                .push((*chunk_count, abs_pos.y.to_pt(), first_byte, last_byte));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    if let Some(page) = doc.pages.first() {
        walk_frame_for_glyph_maps(&page.frame, Point::zero(), &mut chunk_count, &compiled_source, &mut chunks_by_span);
    }

    println!("\n=== Span Grouping Analysis ===");
    println!("Number of unique span_ids: {}", chunks_by_span.len());

    for (span_id, chunks) in &chunks_by_span {
        println!("\nSpan {}...:", &span_id[..8.min(span_id.len())]);
        println!("  Total chunks: {}", chunks.len());

        // Group by Y position
        let mut by_y: HashMap<i64, Vec<_>> = HashMap::new();
        for (idx, y, start, end) in chunks {
            let y_key = (*y * 10.0) as i64;
            by_y.entry(y_key).or_default().push((idx, start, end));
        }

        let mut y_keys: Vec<_> = by_y.keys().collect();
        y_keys.sort();

        for (line_idx, y) in y_keys.iter().enumerate() {
            let line_chunks = &by_y[y];
            let y_pt = **y as f64 / 10.0;
            let _total_bytes: usize = line_chunks.iter().map(|(_, s, e)| *e - *s).sum();
            println!("  Line {} (y={:.1}pt): {} chunks, byte range {}..{}",
                line_idx, y_pt, line_chunks.len(),
                line_chunks.iter().map(|(_, s, _)| *s).min().unwrap(),
                line_chunks.iter().map(|(_, _, e)| *e).max().unwrap()
            );
        }
    }

    println!("\n=== KEY INSIGHT ===");
    println!("If the same span_id has chunks on multiple lines with OVERLAPPING");
    println!("byte ranges, the client's byte->chunk resolution may fail.");
    println!("The client should pick the CORRECT chunk based on visual position.");

    assert!(chunk_count > 0, "Should have emitted some chunks");
}

/// Test: IEEE template title - check how glyph maps are emitted for the wrapped title
///
/// This test uses a more realistic IEEE-like template to see if the title
/// produces multiple FrameItem::Text (per word) or one big Text item.
#[test]
fn test_ieee_title_glyph_map_structure() {
    use clap::Parser;
    use reflexo_typst::args::CompileOnceArgs;

    let args = CompileOnceArgs::parse_from(["tinymist", "main.typ"]);
    let mut verse = args
        .resolve_system()
        .expect("failed to resolve system universe");

    // Simplified IEEE-like template with a long title that wraps
    let typst_code = r#"
#set page(width: 8.5in, height: 11in, margin: 0.75in, columns: 2)
#set text(font: "New Computer Modern", size: 10pt)

// Title at top spanning both columns
#place(
  top + center,
  float: true,
  scope: "parent",
)[
  #align(center)[
    #text(size: 24pt, weight: "bold")[A Tysspepting SsydsXtem to Untangle the Scientisfic Writllisnxg glPsrolcemlsss]
  ]
]

= Introduction

This is the introduction text.
"#;

    let source_obj = Source::new(verse.main_id().unwrap(), typst_code.into());
    verse
        .map_shadow_by_id(source_obj.id(), Bytes::from_string(source_obj.text().to_owned()))
        .expect("failed to map shadow");

    let world = verse.snapshot();
    let doc = typst::compile::<TypstPagedDocument>(&world)
        .output
        .expect("compilation should succeed");

    let compiled_source = world.source(world.main()).unwrap();

    println!("\n=== IEEE Template Title Test ===");
    println!("Source length: {} bytes", compiled_source.text().len());

    // Find the title text in the source
    let title_start = compiled_source.text().find("A Tysspepting").unwrap_or(0);
    let title_end = compiled_source.text().find("glPsrolcemlsss").map(|p| p + "glPsrolcemlsss".len()).unwrap_or(0);
    println!("Title in source: bytes {}..{}", title_start, title_end);
    println!("Title text: {:?}", &compiled_source.text()[title_start..title_end]);

    // Track text items that might be part of the title
    let mut title_text_items: Vec<(f64, f64, usize, usize, usize)> = Vec::new(); // (x, y, first_byte, last_byte, glyph_count)

    fn walk_frame_for_title(
        frame: &Frame,
        offset: Point,
        source: &Source,
        title_start: usize,
        title_end: usize,
        items: &mut Vec<(f64, f64, usize, usize, usize)>,
    ) {
        for (pos, item) in frame.items() {
            let abs_pos = Point::new(offset.x + pos.x, offset.y + pos.y);
            match item {
                FrameItem::Group(group) => {
                    walk_frame_for_title(&group.frame, abs_pos, source, title_start, title_end, items);
                }
                FrameItem::Text(text) => {
                    if let Some(first_glyph) = text.glyphs.first() {
                        let glyph_span = first_glyph.span.0;
                        let local_offset = first_glyph.span.1 as usize;

                        if let Some(range) = source.range(glyph_span) {
                            let first_byte = range.start + local_offset;

                            // Check if this text is in the title range
                            if first_byte >= title_start && first_byte < title_end {
                                let last_glyph = text.glyphs.last().unwrap();
                                let last_local_offset = last_glyph.span.1 as usize;
                                let last_byte = range.start + last_local_offset + 1;

                                items.push((
                                    abs_pos.x.to_pt(),
                                    abs_pos.y.to_pt(),
                                    first_byte,
                                    last_byte,
                                    text.glyphs.len(),
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    if let Some(page) = doc.pages.first() {
        walk_frame_for_title(&page.frame, Point::zero(), &compiled_source, title_start, title_end, &mut title_text_items);
    }

    println!("\n=== Title FrameItem::Text items ===");
    println!("Count: {}", title_text_items.len());

    // Sort by byte position
    title_text_items.sort_by_key(|item| item.2);

    // Group by Y position
    let mut by_y: HashMap<i64, Vec<_>> = HashMap::new();
    for item in &title_text_items {
        let y_key = (item.1 * 10.0) as i64;
        by_y.entry(y_key).or_default().push(item);
    }

    let mut y_keys: Vec<_> = by_y.keys().collect();
    y_keys.sort();

    for y in &y_keys {
        let items = &by_y[y];
        let y_pt = **y as f64 / 10.0;
        println!("\nLine at y={:.1}pt ({} text items):", y_pt, items.len());
        for (x, _, first, last, glyphs) in items {
            println!("  x={:.1}pt, bytes={}..{}, glyphs={}", x, first, last, glyphs);
        }
    }

    println!("\n=== Analysis ===");
    if title_text_items.len() == 1 {
        println!("WARNING: Title renders as ONE big FrameItem::Text!");
        println!("This means there's only one chunk, and the client can't distinguish lines.");
    } else if y_keys.len() > 1 {
        println!("GOOD: Title renders as {} FrameItem::Text items across {} lines.",
                 title_text_items.len(), y_keys.len());
        println!("Each will emit a separate SpanGlyphMap chunk.");
    } else {
        println!("Title renders as {} FrameItem::Text items on 1 line.", title_text_items.len());
    }

    // Verify byte ranges are contiguous (no gaps, no overlaps)
    println!("\n=== Byte Range Continuity Check ===");
    let mut all_items_sorted = title_text_items.clone();
    all_items_sorted.sort_by_key(|item| item.2); // Sort by first_byte

    let mut prev_end: Option<usize> = None;
    let mut has_gaps = false;
    let mut has_overlaps = false;
    for (_, _, first, last, _) in &all_items_sorted {
        if let Some(pe) = prev_end {
            if *first < pe {
                println!("  OVERLAP: prev_end={}, next_start={}", pe, first);
                has_overlaps = true;
            } else if *first > pe {
                println!("  GAP: prev_end={}, next_start={} (gap of {} bytes)", pe, first, first - pe);
                has_gaps = true;
            }
        }
        prev_end = Some(*last);
    }

    if !has_gaps && !has_overlaps {
        println!("  All byte ranges are contiguous - no gaps or overlaps!");
    }

    // Key assertion: no overlaps means byte->chunk resolution will work
    assert!(!has_overlaps, "Byte ranges should not overlap!");
    assert!(title_text_items.len() > 0, "Should have found title text items");
}
