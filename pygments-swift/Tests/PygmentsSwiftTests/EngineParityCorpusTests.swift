import XCTest
@testable import PygmentsSwift

final class EngineParityCorpusTests: XCTestCase {
    struct Fixture {
        let name: String
        let lexerName: String
        let makeSwiftLexer: () -> Lexer
        let input: String
        let compareStartScalar: Bool
    }

    func testEngineParityCorpus() throws {
        // These fixtures are intended to exercise core engine semantics (states, byGroups,
        // delegation, defaults, Unicode handling) using lexers we have strict ports for.
        let fixtures: [Fixture] = [
            Fixture(
                name: "swift_ascii_1",
                lexerName: "swift",
                makeSwiftLexer: { SwiftLexer() },
                input: """
                import Foundation

                struct Foo {
                    let x: Int = 42
                    func bar() -> String { return \"hi\" }
                }

                /* outer /* inner */ done */
                let s = \"value: \\(x)\"\n
                """,
                compareStartScalar: true
            ),
            Fixture(
                name: "swift_unicode_1",
                lexerName: "swift",
                makeSwiftLexer: { SwiftLexer() },
                input: """
                import Foundation

                let cafe = \"café\"
                let combining = \"e\u{0301}\"
                let emoji = \"🙂\"\n
                """,
                compareStartScalar: true
            ),
            Fixture(
                name: "json_ascii_1",
                lexerName: "json",
                makeSwiftLexer: { JsonLexer() },
                input: """
                {
                  // comment
                  \"a\": 1,
                  \"b\": 2.5e+2,
                  \"c\": true,
                  \"d\": null,
                  \"e\": \"str\\n\\t\\u1234\",
                  \"arr\": [1, 2, 3]
                }\n
                """,
                compareStartScalar: true
            ),
            Fixture(
                name: "jsonld_ascii_1",
                lexerName: "jsonld",
                makeSwiftLexer: { JsonLdLexer() },
                input: """
                {
                  \"@context\": {\"name\": \"http://schema.org/name\"},
                  \"@id\": \"http://example.com\",
                  \"name\": \"Alice\"\n
                }\n
                """,
                compareStartScalar: true
            ),
        ]

        for fx in fixtures {
            try runFixture(fx)
        }
    }

    private func runFixture(_ fixture: Fixture) throws {
        let pyTokens = try PythonPygmentsReference.run(input: fixture.input, lexerName: fixture.lexerName)
        let swiftLexer = fixture.makeSwiftLexer()
        let swiftTokens = swiftLexer.getTokens(fixture.input)

        // Compare round-trip text after preprocessing.
        // Both sides should reconstruct identical preprocessed text via concatenating token values.
        let pyText = pyTokens.map { $0.value }.joined()
        let swText = swiftTokens.map { $0.value }.joined()
        XCTAssertEqual(pyText, swText, "\(fixture.name): reconstructed preprocessed text mismatch")

        XCTAssertEqual(pyTokens.count, swiftTokens.count, "\(fixture.name): token count mismatch")

        for idx in 0..<min(pyTokens.count, swiftTokens.count) {
            let py = pyTokens[idx]
            let sw = swiftTokens[idx]

            if py.type != sw.type.description || py.value != sw.value {
                XCTFail("\(fixture.name): mismatch at #\(idx): python=(\(py.type), \(py.value.debugDescription)) swift=(\(sw.type.description), \(sw.value.debugDescription))")
                return
            }

            if fixture.compareStartScalar {
                if py.start != sw.startScalar {
                    XCTFail("\(fixture.name): start mismatch at #\(idx): pythonStart=\(py.start) swiftStartScalar=\(sw.startScalar) value=\(sw.value.debugDescription)")
                    return
                }
            }
        }
    }
}
