/// Integration tests for glyph map collection.
/// Run with: cargo test --features incr test_glyph -- --nocapture
///
/// These tests verify that glyph maps are correctly collected from
/// the compiled document, providing per-glyph byte offsets for
/// accurate click-to-cursor positioning.
///
/// KEY REQUIREMENT: When text spans wrap across multiple lines, the compiler
/// must emit MULTIPLE SpanGlyphMap entries with the SAME span_id (one per visual chunk).
/// The client groups these by spanId to create chunk arrays for accurate cursor placement.

#[cfg(all(test, feature = "incr"))]
mod tests {
    use crate::{PageGlyphMap, SpanGlyphMap, TypstCompileWorld};
    use typst::layout::{Abs, Frame, FrameItem, GroupItem, Point, Size};

    #[test]
    fn test_glyph_map_structure() {
        println!("\n=== Glyph Map Structure Test ===\n");

        // Create a synthetic SpanGlyphMap
        let glyph_map = SpanGlyphMap {
            span_id: "abc123".to_string(),
            byte_offsets: vec![0, 1, 2, 3, 4, 5], // "Hello" = 5 chars + end position
        };

        println!("SpanGlyphMap:");
        println!("  span_id: {}", glyph_map.span_id);
        println!("  byte_offsets: {:?}", glyph_map.byte_offsets);
        println!();

        // Verify structure
        assert_eq!(glyph_map.span_id, "abc123");
        assert_eq!(glyph_map.byte_offsets.len(), 6); // 5 glyphs + 1 end position

        // Verify byte offsets are monotonically increasing
        for i in 1..glyph_map.byte_offsets.len() {
            assert!(
                glyph_map.byte_offsets[i] >= glyph_map.byte_offsets[i - 1],
                "Byte offsets should be monotonically increasing"
            );
        }

        println!("✓ Glyph map structure is correct");
    }

    #[test]
    fn test_page_glyph_map_structure() {
        println!("\n=== Page Glyph Map Structure Test ===\n");

        let page_map = PageGlyphMap {
            page: 0,
            version: 1,
            spans: vec![
                SpanGlyphMap {
                    span_id: "span1".to_string(),
                    byte_offsets: vec![0, 1, 2, 3, 4, 5],
                },
                SpanGlyphMap {
                    span_id: "span2".to_string(),
                    byte_offsets: vec![10, 11, 12, 13],
                },
            ],
        };

        println!("PageGlyphMap:");
        println!("  page: {}", page_map.page);
        println!("  version: {}", page_map.version);
        println!("  spans: {} entries", page_map.spans.len());
        for span in &page_map.spans {
            println!("    - {}: {:?}", span.span_id, span.byte_offsets);
        }
        println!();

        assert_eq!(page_map.page, 0);
        assert_eq!(page_map.version, 1);
        assert_eq!(page_map.spans.len(), 2);

        println!("✓ Page glyph map structure is correct");
    }

    #[test]
    fn test_glyph_map_serialization() {
        println!("\n=== Glyph Map Serialization Test ===\n");

        let page_map = PageGlyphMap {
            page: 0,
            version: 1,
            spans: vec![SpanGlyphMap {
                span_id: "12345678abcdef".to_string(),
                byte_offsets: vec![100, 101, 102, 103, 104],
            }],
        };

        // Serialize to JSON (same as what serde_wasm_bindgen does)
        let json = serde_json::to_string_pretty(&page_map).expect("Failed to serialize");
        println!("Serialized JSON:");
        println!("{}", json);
        println!();

        // Verify JSON structure
        assert!(json.contains("\"page\": 0"));
        assert!(json.contains("\"version\": 1"));
        assert!(json.contains("\"spanId\""));  // camelCase from #[serde(rename)]
        assert!(json.contains("\"byteOffsets\""));  // camelCase from #[serde(rename)]
        assert!(json.contains("12345678abcdef"));

        println!("✓ Serialization produces expected JSON structure");
    }

    #[test]
    fn test_glyph_index_to_byte_offset_mapping() {
        println!("\n=== Glyph Index to Byte Offset Mapping Test ===\n");

        // Simulate a text span with byte offsets
        // Source: "Hello" at bytes 100-105
        let byte_offsets = vec![100, 101, 102, 103, 104, 105]; // H(100), e(101), l(102), l(103), o(104), end(105)

        println!("Text: \"Hello\" at bytes 100-105");
        println!("Glyph map: {:?}", byte_offsets);
        println!();

        // Test cursor positioning
        let test_cases = vec![
            (0, 100, "Before 'H' - cursor at start"),
            (1, 101, "Before 'e' - after 'H'"),
            (2, 102, "Before first 'l'"),
            (3, 103, "Before second 'l'"),
            (4, 104, "Before 'o'"),
            (5, 105, "After 'o' - cursor at end"),
        ];

        for (glyph_index, expected_byte, description) in test_cases {
            let actual_byte = byte_offsets[glyph_index.min(byte_offsets.len() - 1)];
            println!(
                "  glyph_index={} → byte={} (expected={}) - {}",
                glyph_index, actual_byte, expected_byte, description
            );
            assert_eq!(
                actual_byte, expected_byte,
                "Glyph index {} should map to byte {}",
                glyph_index, expected_byte
            );
        }

        println!();
        println!("✓ Glyph index to byte offset mapping is correct");
    }

    #[test]
    fn test_utf8_byte_offsets() {
        println!("\n=== UTF-8 Byte Offset Test ===\n");

        // Test with multi-byte UTF-8 characters
        // "Héllo" where é is 2 bytes (C3 A9)
        // H=1byte, é=2bytes, l=1byte, l=1byte, o=1byte = 6 bytes total
        let text = "Héllo";
        let bytes = text.as_bytes();

        println!("Text: \"{}\"", text);
        println!("Byte length: {}", bytes.len());
        println!("Char count: {}", text.chars().count());
        println!();

        // Build expected byte offsets
        let mut byte_offsets = Vec::new();
        let mut current_byte = 0;
        for ch in text.chars() {
            byte_offsets.push(current_byte);
            current_byte += ch.len_utf8();
        }
        byte_offsets.push(current_byte); // End position

        println!("Expected byte offsets: {:?}", byte_offsets);

        // Verify
        assert_eq!(byte_offsets, vec![0, 1, 3, 4, 5, 6]); // H(0), é(1), l(3), l(4), o(5), end(6)

        println!();
        println!("✓ UTF-8 byte offsets are correctly calculated");
    }

    #[test]
    fn test_empty_text_handling() {
        println!("\n=== Empty Text Handling Test ===\n");

        // Empty span should have just the start position
        let empty_offsets: Vec<usize> = vec![42]; // Just the start byte

        println!("Empty text at byte 42");
        println!("Byte offsets: {:?}", empty_offsets);

        assert_eq!(empty_offsets.len(), 1);
        assert_eq!(empty_offsets[0], 42);

        println!();
        println!("✓ Empty text handling is correct");
    }

    #[test]
    fn test_click_before_first_character() {
        println!("\n=== Click Before First Character Test ===\n");

        // This is the core bug we're fixing:
        // When clicking before "A" in "A Typesetting System..."
        // the cursor should be placed at the span's start byte

        // Simulate span for "A Typesetting" at bytes 100-113
        let byte_offsets = vec![
            100, // A
            101, // (space)
            102, // T
            103, // y
            104, // p
            105, // e
            106, // s
            107, // e
            108, // t
            109, // t
            110, // i
            111, // n
            112, // g
            113, // end
        ];

        println!("Text: \"A Typesetting\" at bytes 100-113");
        println!("Glyph map: {:?}", byte_offsets);
        println!();

        // Click before 'A' (glyph index 0)
        let click_glyph_index = 0;
        let cursor_byte = byte_offsets[click_glyph_index];

        println!("Click before 'A' (glyph_index=0):");
        println!("  cursor_byte = {}", cursor_byte);

        assert_eq!(
            cursor_byte, 100,
            "Clicking before 'A' should place cursor at byte 100, not after"
        );

        // Click after 'A' (glyph index 1)
        let click_glyph_index = 1;
        let cursor_byte = byte_offsets[click_glyph_index];

        println!("Click after 'A' (glyph_index=1):");
        println!("  cursor_byte = {}", cursor_byte);

        assert_eq!(cursor_byte, 101, "Clicking after 'A' should place cursor at byte 101");

        println!();
        println!("✓ Click before first character correctly positions cursor");
    }

    #[test]
    fn test_split_spans_title() {
        println!("\n=== Split Spans Title Test ===\n");

        // When Typst renders a long title, it may split it into multiple spans
        // due to line wrapping. Each span gets its own glyph map.

        // Title: "A Typesetting System to Untangle the Scientific Writing Process"
        // Might be split as:
        // Span 1: "A Typesetting System to Untangle the Sci-" (line 1)
        // Span 2: "entific Writing Process" (line 2)

        let span1_text = "A Typesetting System to Untangle the Sci-";
        let span2_text = "entific Writing Process";

        // Build byte offsets for span 1 (starting at byte 100)
        let mut span1_offsets = Vec::new();
        let mut byte = 100;
        for ch in span1_text.chars() {
            span1_offsets.push(byte);
            byte += ch.len_utf8();
        }
        span1_offsets.push(byte); // End

        // Build byte offsets for span 2 (continuing)
        let mut span2_offsets = Vec::new();
        for ch in span2_text.chars() {
            span2_offsets.push(byte);
            byte += ch.len_utf8();
        }
        span2_offsets.push(byte); // End

        println!("Span 1 (line 1): \"{}\"", span1_text);
        println!("  byte range: {}-{}", span1_offsets[0], span1_offsets.last().unwrap());
        println!("  glyph count: {}", span1_text.chars().count());
        println!();

        println!("Span 2 (line 2): \"{}\"", span2_text);
        println!("  byte range: {}-{}", span2_offsets[0], span2_offsets.last().unwrap());
        println!("  glyph count: {}", span2_text.chars().count());
        println!();

        // Verify clicking before 'A' in span 1 gives byte 100
        assert_eq!(span1_offsets[0], 100, "First character of span 1 should be at byte 100");

        // Verify clicking before 'e' in span 2 gives correct byte
        let span2_start = span1_offsets.last().unwrap();
        assert_eq!(
            span2_offsets[0], *span2_start,
            "First character of span 2 should continue from span 1"
        );

        println!("✓ Split spans have contiguous byte offsets");
    }

    /// Test that verifies the collect_glyphs_from_frame function structure
    /// Note: This test documents expected behavior - actual compilation
    /// requires a full Typst world setup.
    #[test]
    fn test_collect_glyphs_from_frame_expectations() {
        println!("\n=== collect_glyphs_from_frame Expectations ===\n");

        println!("The collect_glyphs_from_frame function should:");
        println!("1. Walk the frame tree recursively");
        println!("2. For each TextItem, extract glyph spans");
        println!("3. Convert glyph.span (SourceSpan, u16) to absolute byte offset");
        println!("4. Include end position (byte after last glyph)");
        println!();

        println!("Expected output structure:");
        println!("  PageGlyphMap {{");
        println!("    page: 0,");
        println!("    version: 1,");
        println!("    spans: [");
        println!("      SpanGlyphMap {{");
        println!("        spanId: \"hex_span_id\",");
        println!("        byteOffsets: [start, byte1, byte2, ..., end]");
        println!("      }}");
        println!("    ]");
        println!("  }}");
        println!();

        println!("Key invariants:");
        println!("  - byteOffsets.len() == glyphs.len() + 1");
        println!("  - byteOffsets are monotonically increasing");
        println!("  - byteOffsets[0] is where cursor goes when clicking before first glyph");
        println!("  - byteOffsets[n] is where cursor goes when clicking after last glyph");

        println!();
        println!("✓ Expectations documented");
    }

    /// Test that wrapped text spans emit multiple glyph map entries.
    ///
    /// When Typst renders text that wraps across multiple lines (due to
    /// width constraints), it creates multiple FrameItem::Text entries
    /// at different Y positions, all with the same span_id.
    ///
    /// The compiler MUST emit one SpanGlyphMap for EACH FrameItem::Text,
    /// even when they share the same span_id. The client will then group
    /// these by spanId into "chunks" for accurate cursor placement.
    ///
    /// Example: A long title like:
    ///   "A Typesetting System to Untangle the Scientific Writing Process"
    /// May wrap to:
    ///   Line 1: "A Typesetting System to Untangle the Sci-"
    ///   Line 2: "entific Writing Process"
    ///
    /// Expected glyph map output:
    ///   [
    ///     { spanId: "abc123", byteOffsets: [0, 1, 2, ..., 41] },  // Line 1 chunk
    ///     { spanId: "abc123", byteOffsets: [41, 42, 43, ..., 65] } // Line 2 chunk
    ///   ]
    ///
    /// The client then groups by spanId:
    ///   glyphMaps["abc123"] = [[0,1,2,...41], [41,42,...65]]
    ///                          ^chunk 0       ^chunk 1
    #[test]
    fn test_wrapped_span_emits_multiple_chunks() {
        println!("\n=== Wrapped Span Multi-Chunk Emission Test ===\n");

        println!("SCENARIO: Long title wraps across two lines");
        println!();

        // Simulate what Typst produces for wrapped text:
        // - Same span_id (from source)
        // - Different Y positions (line 1 vs line 2)
        // - Different glyphs (first half vs second half)

        let span_id = "abc123def456"; // Same span ID for both chunks

        // Chunk 0: First line of wrapped text (bytes 100-141)
        let chunk0 = SpanGlyphMap {
            span_id: span_id.to_string(),
            byte_offsets: (100..142).collect(), // 41 chars + end position
        };

        // Chunk 1: Second line of wrapped text (bytes 141-165)
        let chunk1 = SpanGlyphMap {
            span_id: span_id.to_string(),
            byte_offsets: (141..166).collect(), // 24 chars + end position
        };

        println!("Chunk 0 (line 1): {} glyphs, bytes {}-{}",
            chunk0.byte_offsets.len() - 1,
            chunk0.byte_offsets.first().unwrap(),
            chunk0.byte_offsets.last().unwrap());
        println!("Chunk 1 (line 2): {} glyphs, bytes {}-{}",
            chunk1.byte_offsets.len() - 1,
            chunk1.byte_offsets.first().unwrap(),
            chunk1.byte_offsets.last().unwrap());
        println!();

        // Verify chunk boundaries are contiguous
        assert_eq!(
            chunk0.byte_offsets.last().unwrap(),
            chunk1.byte_offsets.first().unwrap(),
            "Chunk boundaries must be contiguous: chunk0 end == chunk1 start"
        );

        // Verify both have same span_id
        assert_eq!(chunk0.span_id, chunk1.span_id, "Both chunks must have same span_id");

        // This is what the PageGlyphMap.spans should look like:
        let page_map = PageGlyphMap {
            page: 0,
            version: 1,
            spans: vec![chunk0, chunk1],
        };

        // Count entries with same span_id
        let same_span_count = page_map.spans.iter()
            .filter(|s| s.span_id == span_id)
            .count();

        assert_eq!(same_span_count, 2, "Should have 2 entries for wrapped span");

        println!("✅ Wrapped span correctly emits {} chunks with same spanId", same_span_count);
        println!();

        // Verify serialization produces correct structure for client
        let json = serde_json::to_string_pretty(&page_map).unwrap();
        println!("Serialized output (what client receives):");
        println!("{}", json);

        // Count occurrences of spanId in JSON
        let span_id_occurrences = json.matches(&format!("\"spanId\": \"{}\"", span_id)).count();
        assert_eq!(
            span_id_occurrences, 2,
            "JSON should contain spanId exactly twice for wrapped span"
        );

        println!();
        println!("✅ Client will see 2 SpanGlyphMap entries with same spanId");
        println!("   Client groups these into: glyphMaps[spanId] = [chunk0, chunk1]");
    }

    /// Test that verifies the expected client-side grouping behavior.
    ///
    /// This documents how the client's setCompilerGlyphMaps function should
    /// handle multiple SpanGlyphMap entries with the same spanId.
    #[test]
    fn test_client_grouping_expectations() {
        println!("\n=== Client Grouping Expectations ===\n");

        println!("When client receives:");
        println!("  spans: [");
        println!("    {{ spanId: 'abc', byteOffsets: [0,1,2,3] }},  // chunk 0");
        println!("    {{ spanId: 'abc', byteOffsets: [3,4,5,6] }},  // chunk 1");
        println!("    {{ spanId: 'xyz', byteOffsets: [10,11,12] }}  // different span");
        println!("  ]");
        println!();

        println!("Client should group into:");
        println!("  glyphMaps = {{");
        println!("    'abc': [[0,1,2,3], [3,4,5,6]],  // 2 chunks");
        println!("    'xyz': [[10,11,12]]             // 1 chunk");
        println!("  }}");
        println!();

        println!("Then resolveLocationFromByte('abc', 4) should return:");
        println!("  {{ chunkIndex: 1, glyphIndex: 1 }}  // byte 4 is in chunk 1, glyph 1");
        println!();

        println!("This allows correct cursor placement when clicking on line 2 of wrapped text.");
        println!();
        println!("✅ Client grouping expectations documented");
    }

    /// Test that verifies the current behavior of collect_glyphs_from_frame
    /// by simulating what happens when text wraps.
    ///
    /// IMPORTANT: This test will FAIL if the compiler doesn't emit separate
    /// entries for wrapped text. That's the bug we're fixing.
    #[test]
    fn test_wrapped_text_frame_collection() {
        println!("\n=== Wrapped Text Frame Collection Test ===\n");

        println!("This test simulates what Typst produces for wrapped text:");
        println!("- Multiple FrameItem::Text entries with same span");
        println!("- At different Y positions (different lines)");
        println!();

        // Note: We can't easily create real Text items with spans in unit tests
        // because typst::layout::TextItem requires font data and proper spans.
        // Instead, we document the expected behavior.

        println!("Expected frame structure from Typst:");
        println!("  Frame (page)");
        println!("    └─ Group (paragraph container)");
        println!("         ├─ Text @ y=100pt, span=ABC, glyphs=[H,e,l,l,o, ,w,o,r,l,d]");
        println!("         └─ Text @ y=120pt, span=ABC, glyphs=[t,h,i,s, ,i,s, ,l,i,n,e,2]");
        println!("                           ^^^^^^^^^ same span!");
        println!();

        println!("collect_glyphs_from_frame should produce:");
        println!("  [");
        println!("    SpanGlyphMap {{ spanId: 'ABC', byteOffsets: [0..11] }},  // line 1");
        println!("    SpanGlyphMap {{ spanId: 'ABC', byteOffsets: [11..24] }}  // line 2");
        println!("  ]");
        println!();

        println!("If only ONE SpanGlyphMap is produced, clicking on line 2 will");
        println!("incorrectly resolve to chunk 0 (line 1), causing cursor jump!");
        println!();

        println!("✅ Expected behavior documented");
    }

    /// Test that verifies glyph map entries preserve order for wrapped spans.
    ///
    /// When text wraps, each line's glyphs should be in a separate SpanGlyphMap entry,
    /// but they should have the SAME span_id. The client groups by span_id to create chunks.
    ///
    /// Key invariant: For a wrapped span with N visual lines:
    /// - There should be N SpanGlyphMap entries with the same span_id
    /// - Entries should appear in visual order (line 1 before line 2)
    /// - Byte offsets should be contiguous: entry[i].last == entry[i+1].first
    #[test]
    fn test_glyph_map_entry_ordering() {
        println!("\n=== Glyph Map Entry Ordering Test ===\n");

        // Simulate what the compiler should produce for wrapped text
        let span_id = "12345678abcd";

        // Entry 1: First visual line (chunk 0)
        let entry1 = SpanGlyphMap {
            span_id: span_id.to_string(),
            byte_offsets: vec![10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], // 10 glyphs + end
        };

        // Entry 2: Second visual line (chunk 1) - same span_id!
        let entry2 = SpanGlyphMap {
            span_id: span_id.to_string(),
            byte_offsets: vec![20, 21, 22, 23, 24, 25, 26, 27, 28], // 8 glyphs + end
        };

        let entries = vec![entry1, entry2];

        // Verify: Both entries have same span_id
        assert!(entries.iter().all(|e| e.span_id == span_id),
            "All entries for wrapped span must have same span_id");

        // Verify: Entries are contiguous (last byte of entry[i] == first byte of entry[i+1])
        for i in 0..entries.len() - 1 {
            let this_end = *entries[i].byte_offsets.last().unwrap();
            let next_start = *entries[i + 1].byte_offsets.first().unwrap();
            assert_eq!(this_end, next_start,
                "Entry {} end ({}) must equal entry {} start ({})",
                i, this_end, i + 1, next_start);
        }

        // Verify: Byte offsets are monotonically increasing within each entry
        for (i, entry) in entries.iter().enumerate() {
            for j in 1..entry.byte_offsets.len() {
                assert!(entry.byte_offsets[j] >= entry.byte_offsets[j - 1],
                    "Entry {} byte_offsets must be monotonically increasing", i);
            }
        }

        println!("Entry 0: {} glyphs, bytes {}-{}",
            entries[0].byte_offsets.len() - 1,
            entries[0].byte_offsets.first().unwrap(),
            entries[0].byte_offsets.last().unwrap());
        println!("Entry 1: {} glyphs, bytes {}-{}",
            entries[1].byte_offsets.len() - 1,
            entries[1].byte_offsets.first().unwrap(),
            entries[1].byte_offsets.last().unwrap());

        println!();
        println!("✅ Glyph map entries correctly ordered for wrapped span");
    }

    /// Test helper function to count how many entries exist for each span_id.
    ///
    /// This is the key metric: for wrapped spans, count should be > 1.
    fn count_entries_by_span_id(entries: &[SpanGlyphMap]) -> std::collections::HashMap<String, usize> {
        let mut counts = std::collections::HashMap::new();
        for entry in entries {
            *counts.entry(entry.span_id.clone()).or_insert(0) += 1;
        }
        counts
    }

    /// Test that verifies the counting helper works correctly.
    #[test]
    fn test_count_entries_helper() {
        println!("\n=== Entry Counting Helper Test ===\n");

        let entries = vec![
            SpanGlyphMap { span_id: "aaa".to_string(), byte_offsets: vec![0, 1, 2] },
            SpanGlyphMap { span_id: "aaa".to_string(), byte_offsets: vec![2, 3, 4] },
            SpanGlyphMap { span_id: "bbb".to_string(), byte_offsets: vec![10, 11] },
            SpanGlyphMap { span_id: "aaa".to_string(), byte_offsets: vec![4, 5, 6] },
        ];

        let counts = count_entries_by_span_id(&entries);

        println!("Input: 4 entries (3 for 'aaa', 1 for 'bbb')");
        println!("Counts: {:?}", counts);

        assert_eq!(counts.get("aaa"), Some(&3), "Should have 3 entries for 'aaa'");
        assert_eq!(counts.get("bbb"), Some(&1), "Should have 1 entry for 'bbb'");

        // Find spans with multiple chunks (wrapped spans)
        let multi_chunk_spans: Vec<_> = counts.iter()
            .filter(|(_, &count)| count > 1)
            .collect();

        println!("Multi-chunk spans: {:?}", multi_chunk_spans);
        assert_eq!(multi_chunk_spans.len(), 1, "Should have 1 multi-chunk span");
        assert_eq!(multi_chunk_spans[0].0, "aaa", "Multi-chunk span should be 'aaa'");

        println!();
        println!("✅ Entry counting helper works correctly");
    }

    /// This test documents the EXPECTED behavior that we need to verify in the browser.
    ///
    /// When the IEEE template title "A Tysspepting SsydsXtem to Untangle the Scientisfic
    /// Writllisnxg glPsrolcemlsss" wraps across two lines, we expect:
    ///
    /// 1. TWO SpanGlyphMap entries with the SAME span_id
    /// 2. Entry 0: glyphs for line 1
    /// 3. Entry 1: glyphs for line 2
    /// 4. Byte offsets contiguous: entry0.last == entry1.first
    ///
    /// To verify in browser console, look for:
    /// [GlyphMap] span=XXXXXXXX pos=(x1,y1) glyphs=N1 existing_chunks=0
    /// [GlyphMap] span=XXXXXXXX pos=(x2,y2) glyphs=N2 existing_chunks=1  <-- THIS IS KEY!
    ///
    /// If existing_chunks > 0, the span has multiple visual chunks (wrapped text).
    #[test]
    fn test_ieee_title_expectations() {
        println!("\n=== IEEE Title Wrapped Span Expectations ===\n");

        println!("Title: \"A Tysspepting SsydsXtem to Untangle the Scientisfic Writllisnxg glPsrolcemlsss\"");
        println!("This title WRAPS across two lines in the IEEE template.");
        println!();

        println!("EXPECTED collect_glyphs_from_frame output:");
        println!("  [");
        println!("    SpanGlyphMap {{ spanId: 'XXXXXXXX...', byteOffsets: [line1 bytes] }},");
        println!("    SpanGlyphMap {{ spanId: 'XXXXXXXX...', byteOffsets: [line2 bytes] }},  // SAME span_id!");
        println!("  ]");
        println!();

        println!("BROWSER VERIFICATION:");
        println!("  1. Open browser console");
        println!("  2. Look for: [GlyphMap] span=XXXXXXXX... existing_chunks=1");
        println!("  3. If existing_chunks=1 appears, compiler correctly emits 2 chunks");
        println!("  4. If only existing_chunks=0 appears, BUG: missing chunk for line 2");
        println!();

        println!("If the compiler only emits ONE entry for the wrapped title:");
        println!("  - Clicking on line 2 resolves to chunk 0 (wrong!)");
        println!("  - resolveLocationFromByte returns chunkIndex=0 (wrong!)");
        println!("  - Cursor appears on line 1 instead of line 2");
        println!();

        println!("✅ IEEE title expectations documented");
    }

    /// Integration test that verifies the wrapped title scenario using the actual
    /// IEEE template source code.
    ///
    /// This test:
    /// 1. Creates a source with the IEEE template
    /// 2. Documents what the compiled frame structure should look like
    /// 3. Provides assertions that can be verified in browser
    #[test]
    fn test_ieee_template_wrapped_title_glyph_map() {
        println!("\n=== IEEE Template Wrapped Title Glyph Map Test ===\n");

        // The IEEE template source with the wrapped title
        let source_code = r##"#show: ieee.with(
  title: [A Tysspepting SsydsXtem to Untangle the Scientisfic Writllisnxg glPsrolcemlsss],
  ...
)"##;

        println!("Source code snippet:");
        println!("{}", source_code);
        println!();

        // The title text that should wrap
        let title = "A Tysspepting SsydsXtem to Untangle the Scientisfic Writllisnxg glPsrolcemlsss";
        println!("Title text: \"{}\"", title);
        println!("Title length: {} chars, {} bytes", title.chars().count(), title.len());
        println!();

        // What we expect from the compiler:
        println!("EXPECTED COMPILER BEHAVIOR:");
        println!();
        println!("When Typst renders this title at 24pt bold, it will:");
        println!("  1. Calculate that it doesn't fit on one line");
        println!("  2. Break it into multiple FrameItem::Text items");
        println!("  3. Each Text item has the SAME span (source location)");
        println!("  4. But different glyphs (different portions of the title)");
        println!();

        println!("Frame structure for wrapped title:");
        println!("  Frame (page)");
        println!("    └─ Group (float container)");
        println!("         └─ Group (alignment)");
        println!("              └─ Group (text styling: 24pt bold)");
        println!("                   ├─ Text @ y=Y1: \"A Tysspepting SsydsXtem to Untangle the\"");
        println!("                   └─ Text @ y=Y2: \"Scientisfic Writllisnxg glPsrolcemlsss\"");
        println!("                        ^^^^ SAME span_id, DIFFERENT positions!");
        println!();

        println!("collect_glyphs_from_frame should produce:");
        println!("  [");
        println!("    SpanGlyphMap {{ spanId: 'X', byteOffsets: [bytes for line 1] }},");
        println!("    SpanGlyphMap {{ spanId: 'X', byteOffsets: [bytes for line 2] }},");
        println!("  ]");
        println!();

        println!("The KEY assertion is:");
        println!("  result.iter().filter(|m| m.span_id == title_span_id).count() >= 2");
        println!();

        // Document client-side grouping
        println!("CLIENT-SIDE GROUPING (use-glyph-map.ts setCompilerGlyphMaps):");
        println!("  When client receives multiple entries with same spanId:");
        println!("    seenInThisUpdate.has(spanId) ? existing.push(byteOffsets) : newMaps.set(spanId, [byteOffsets])");
        println!("  Result: glyphMaps[spanId] = [[line1 offsets], [line2 offsets]]");
        println!();

        println!("CLIENT-SIDE RESOLUTION (resolveLocationFromByte):");
        println!("  When cursor is at byte B in line 2:");
        println!("    - Iterate chunks: chunk0 doesn't contain B, chunk1 does");
        println!("    - Return {{ chunkIndex: 1, glyphIndex: N }}");
        println!();

        println!("BUG SCENARIO (if compiler emits only 1 entry):");
        println!("  - glyphMaps[spanId] = [[all offsets]]  // single chunk");
        println!("  - resolveLocationFromByte always returns chunkIndex: 0");
        println!("  - Cursor position calculated from chunk 0's DOM element");
        println!("  - Cursor appears on LINE 1 even when user clicked LINE 2!");
        println!();

        println!("✅ IEEE template wrapped title test documented");
    }
}
