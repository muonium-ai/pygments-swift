import Foundation
import XCTest
@testable import PygmentsSwift

final class SwiftLexerParityTests: XCTestCase {
    func testParityWithPythonPygmentsOnAsciiSample() throws {
        // This parity test intentionally uses ASCII-only input so that Python's
        // character indices align with our current UTF-16 offsets.
        let input = """
        import Foundation

        struct Foo {
            let x: Int = 42
            func bar() -> String { return \"hi\" }
        }

        /* outer /* inner */ done */
        let s = \"value: \\(x)\"
        """

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input)
        let lexer = SwiftLexer()
        let swiftTokens = lexer.getTokens(input)

        // Always verify round-tripping the preprocessed text.
        let preprocessed = lexer.preprocess(input)
        XCTAssertEqual(pyTokens.map { $0.value }.joined(), preprocessed)
        XCTAssertEqual(swiftTokens.map { $0.value }.joined(), preprocessed)

        let strictEnv = ProcessInfo.processInfo.environment["PYGMENTS_STRICT_PARITY"]
        let strict = (strictEnv == nil) || (strictEnv == "1")
        if strict {
            // Strict: compare full (type, value) streams (ASCII-only input keeps indices aligned enough).
            let pyPairs = pyTokens.map { ($0.type, $0.value) }
            let swiftPairs = swiftTokens.map { ($0.type.description, $0.value) }
            XCTAssertEqual(pyPairs.count, swiftPairs.count, "Token stream lengths differ")
            for idx in 0..<min(pyPairs.count, swiftPairs.count) {
                let (pyType, pyValue) = pyPairs[idx]
                let (swType, swValue) = swiftPairs[idx]
                if pyType != swType || pyValue != swValue {
                    XCTFail("Mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                    return
                }
            }
        } else {
            // Default (non-blocking): compare a normalized stream with whitespace removed.
            // This gives us signal while we fill in missing builtins and edge cases.
            let pyPairs = pyTokens
                .filter { !$0.type.hasPrefix("Token.Text") && !$0.type.hasPrefix("Token.Whitespace") }
                .map { (normalizeType($0.type), $0.value) }

            let swiftPairs = swiftTokens
                .filter { !$0.type.isSubtype(of: .text) && !$0.type.isSubtype(of: .whitespace) }
                .map { (normalizeType($0.type.description), $0.value) }

            XCTAssertEqual(pyPairs.count, swiftPairs.count, "Normalized token stream lengths differ")
            for idx in 0..<min(pyPairs.count, swiftPairs.count) {
                let (pyType, pyValue) = pyPairs[idx]
                let (swType, swValue) = swiftPairs[idx]
                if pyType != swType || pyValue != swValue {
                    XCTFail("Normalized mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                    return
                }
            }
        }
    }

    func testParityWithPythonPygmentsOnAsciiSampleMoreStates() throws {
        // ASCII-only input: keeps Python indices aligned with UTF-16 offsets.
        // This sample tries to touch more SwiftLexer states without relying on
        // version-specific builtins.
        let input = """
        #if available(iOS 13, *)
        import Foundation
        #else
        import Foundation
        #endif

        // MARK: parity
        /* TODO: make sure nested comments work
           outer /* inner */ done
        */

        struct Foo {
            let x: Int = 42
            var y: Int = 0b1010_0011
            let z: Int = 0xFF_EE

            func bar() -> String {
                let s = "escapes: \\n \\t \\u{1234} value=\\(x)"
                return s
            }
        }
        """

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input)
        let lexer = SwiftLexer()
        let swiftTokens = lexer.getTokens(input)

        let preprocessed = lexer.preprocess(input)
        XCTAssertEqual(pyTokens.map { $0.value }.joined(), preprocessed)
        XCTAssertEqual(swiftTokens.map { $0.value }.joined(), preprocessed)

        // Keep parity strict for ASCII samples.
        let pyPairs = pyTokens.map { ($0.type, $0.value) }
        let swiftPairs = swiftTokens.map { ($0.type.description, $0.value) }
        XCTAssertEqual(pyPairs.count, swiftPairs.count, "Token stream lengths differ")
        for idx in 0..<min(pyPairs.count, swiftPairs.count) {
            let (pyType, pyValue) = pyPairs[idx]
            let (swType, swValue) = swiftPairs[idx]
            if pyType != swType || pyValue != swValue {
                XCTFail("Mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                return
            }
        }
    }

    func testParityStartScalarOnUnicodeSample() throws {
        // Includes non-ASCII identifiers and strings.
        let input = """
        import Foundation

        let cafe = \"café\"
        let naive = \"naïve\"
        // Combining character: e + ◌́ (U+0301)
        let combining = \"e\u{0301}\"
        let emoji = \"🙂\"
        """

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input)
        let lexer = SwiftLexer()
        let swiftTokens = lexer.getTokens(input)

        // Compare full (type, value) stream, and compare python start to swift startScalar.
        XCTAssertEqual(pyTokens.count, swiftTokens.count, "Token stream lengths differ")

        for idx in 0..<min(pyTokens.count, swiftTokens.count) {
            let py = pyTokens[idx]
            let sw = swiftTokens[idx]
            if py.type != sw.type.description || py.value != sw.value {
                XCTFail("Mismatch at #\\(idx): python=(\\(py.type), \\(py.value.debugDescription)) swift=(\\(sw.type.description), \\(sw.value.debugDescription))")
                return
            }
            if py.start != sw.startScalar {
                XCTFail("Start mismatch at #\\(idx): pythonStart=\\(py.start) swiftStartScalar=\\(sw.startScalar) value=\\(sw.value.debugDescription)")
                return
            }
        }
    }

    func testParityWithPythonPygmentsOnAsciiSampleRawAndMultilineStrings() throws {
        // ASCII-only sample: keeps Python indices aligned with UTF-16 offsets.
        // Use normalized (non-strict) comparison to keep this corpus expansion
        // resilient across minor Pygments lexer variations.
        let input = ##"""
        struct S {
            let x: Int = 42

            // Raw string literal (Swift 5+): backslashes are mostly literal.
            let raw = #"raw \n \t"#

            // Interpolation inside raw strings uses \#(...)
            let interp = #"value=\#(x)"#

            // Multiline string literal
            let multi = """
            line1
            line2 \(x)
            """
        }
        """##

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input)
        let lexer = SwiftLexer()
        let swiftTokens = lexer.getTokens(input)

        let preprocessed = lexer.preprocess(input)
        XCTAssertEqual(pyTokens.map { $0.value }.joined(), preprocessed)
        XCTAssertEqual(swiftTokens.map { $0.value }.joined(), preprocessed)

        let pyPairs = pyTokens
            .filter { !$0.type.hasPrefix("Token.Text") && !$0.type.hasPrefix("Token.Whitespace") }
            .map { (normalizeType($0.type), $0.value) }

        let swiftPairs = swiftTokens
            .filter { !$0.type.isSubtype(of: TokenType.text) && !$0.type.isSubtype(of: TokenType.whitespace) }
            .map { (normalizeType($0.type.description), $0.value) }

        XCTAssertEqual(pyPairs.count, swiftPairs.count, "Normalized token stream lengths differ")
        for idx in 0..<min(pyPairs.count, swiftPairs.count) {
            let (pyType, pyValue) = pyPairs[idx]
            let (swType, swValue) = swiftPairs[idx]
            if pyType != swType || pyValue != swValue {
                XCTFail("Normalized mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                return
            }
        }
    }

    func testJsonParityWithPythonPygmentsOnAsciiSample() throws {
        // ASCII-only sample so python indices align with UTF-16 offsets.
        let input = """
        {
          // comment
          "a": 1,
          "b": 2.5e+2,
          "c": true,
          "d": null,
          "e": "str\\n\\t\\u1234",
          "arr": [1, 2, 3]
        }
        """

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input, lexerName: "json")
        let lexer = JsonLexer()
        let swiftTokens = lexer.getTokens(input)

        let preprocessed = lexer.preprocess(input)
        XCTAssertEqual(pyTokens.map { $0.value }.joined(), preprocessed)
        XCTAssertEqual(swiftTokens.map { $0.value }.joined(), preprocessed)

        let pyPairs = pyTokens.map { ($0.type, $0.value) }
        let swiftPairs = swiftTokens.map { ($0.type.description, $0.value) }
        XCTAssertEqual(pyPairs.count, swiftPairs.count, "Token stream lengths differ")
        for idx in 0..<min(pyPairs.count, swiftPairs.count) {
            let (pyType, pyValue) = pyPairs[idx]
            let (swType, swValue) = swiftPairs[idx]
            if pyType != swType || pyValue != swValue {
                XCTFail("Mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                return
            }
        }
    }

    func testJsonLdParityWithPythonPygmentsOnAsciiSample() throws {
        let input = """
        {
          "@context": {"name": "http://schema.org/name"},
          "@id": "http://example.com",
          "name": "Alice"
        }
        """

        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let pyTokens = try runPythonReference(python: python, input: input, lexerName: "jsonld")
        let lexer = JsonLdLexer()
        let swiftTokens = lexer.getTokens(input)

        let preprocessed = lexer.preprocess(input)
        XCTAssertEqual(pyTokens.map { $0.value }.joined(), preprocessed)
        XCTAssertEqual(swiftTokens.map { $0.value }.joined(), preprocessed)

        let pyPairs = pyTokens.map { ($0.type, $0.value) }
        let swiftPairs = swiftTokens.map { ($0.type.description, $0.value) }
        XCTAssertEqual(pyPairs.count, swiftPairs.count, "Token stream lengths differ")
        for idx in 0..<min(pyPairs.count, swiftPairs.count) {
            let (pyType, pyValue) = pyPairs[idx]
            let (swType, swValue) = swiftPairs[idx]
            if pyType != swType || pyValue != swValue {
                XCTFail("Mismatch at #\\(idx): python=(\\(pyType), \\(pyValue.debugDescription)) swift=(\\(swType), \\(swValue.debugDescription))")
                return
            }
        }
    }

    private func normalizeType(_ s: String) -> String {
        // Collapse subtypes to their top-level families.
        // Example: Token.Name.Builtin -> Token.Name
        let prefixes = [
            "Token.Keyword",
            "Token.Name",
            "Token.Literal",
            "Token.String",
            "Token.Number",
            "Token.Comment",
            "Token.Operator",
            "Token.Punctuation",
            "Token.Generic",
            "Token.Error",
            "Token.Text",
            "Token.Whitespace",
        ]
        for p in prefixes {
            if s == p || s.hasPrefix(p + ".") { return p }
        }
        return s
    }

    // MARK: - Helpers

    private typealias RefToken = PythonPygmentsReference.RefToken

    private func findPython() -> String? {
        PythonPygmentsReference.findPython()
    }

    private func runPythonReference(python: String, input: String, lexerName: String? = nil) throws -> [RefToken] {
        let name = lexerName ?? "swift"
        return try PythonPygmentsReference.run(input: input, lexerName: name)
    }
}
