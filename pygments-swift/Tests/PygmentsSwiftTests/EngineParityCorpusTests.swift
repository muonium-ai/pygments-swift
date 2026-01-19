import XCTest
@testable import PygmentsSwift

final class EngineParityCorpusTests: XCTestCase {
    // MARK: - Swift-side custom lexers for engine parity

    private final class CustomIncludePrecedenceLexer: RegexLexer {
        override var tokenDefs: [String: [TokenRuleDef]] {
            [
                "root": [
                    .rule(Rule("\\n", action: .token(.whitespace))),
                    .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                    .include("inc"),
                    .rule(Rule("x", action: .token(.keyword))),
                ],
                "inc": [
                    .rule(Rule("x", action: .token(.name))),
                ],
            ]
        }
    }

    private final class CustomDefaultPopLexer: RegexLexer {
        override var tokenDefs: [String: [TokenRuleDef]] {
            [
                "root": [
                    .rule(Rule("\\n", action: .token(.whitespace))),
                    .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                    .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.push("inner")]))),
                    .rule(Rule("[a-zA-Z_]+", action: .token(.name))),
                ],
                "inner": [
                    .default(.ops([.pop]))
                ],
            ]
        }
    }

    private final class CustomNoMatchNewlineResetLexer: RegexLexer {
        override var tokenDefs: [String: [TokenRuleDef]] {
            [
                "root": [
                    .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                    .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.push("inner")]))),
                    .rule(Rule("a", action: .token(.name))),
                ],
                "inner": [
                    // Intentionally no rule for "\\n".
                    .rule(Rule("a", action: .token(.keyword))),
                ]
            ]
        }
    }

    private struct FixtureManifest: Decodable {
        let name: String
        let lexerName: String
        let swiftLexer: String
        let inputResource: String
        let inputExtension: String
        let compareStartScalar: Bool
    }

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
        let fixtures = try loadFixtures()
        XCTAssertFalse(fixtures.isEmpty, "Engine parity corpus has no fixtures")

        for fx in fixtures {
            try runFixture(fx)
        }
    }

    private func loadFixtures() throws -> [Fixture] {
        let manifestURL = try requireResourceURL(resource: "engine_parity", ext: "json")
        let manifestData = try Data(contentsOf: manifestURL)
        let manifests = try JSONDecoder().decode([FixtureManifest].self, from: manifestData)

        return try manifests.map { entry in
            let inputURL = try requireResourceURL(resource: entry.inputResource, ext: entry.inputExtension, subdirectory: "EngineParity")
            let input = try String(contentsOf: inputURL, encoding: .utf8)
            return Fixture(
                name: entry.name,
                lexerName: entry.lexerName,
                makeSwiftLexer: { self.makeLexer(entry.swiftLexer) },
                input: input,
                compareStartScalar: entry.compareStartScalar
            )
        }
    }

    private func makeLexer(_ key: String) -> Lexer {
        switch key.lowercased() {
        case "swift":
            return SwiftLexer()
        case "json":
            return JsonLexer()
        case "jsonld":
            return JsonLdLexer()
        case "custom_include_precedence":
            return CustomIncludePrecedenceLexer()
        case "custom_default_pop":
            return CustomDefaultPopLexer()
        case "custom_nomatch_newline_reset":
            return CustomNoMatchNewlineResetLexer()
        default:
            // Keep this strict: adding a new corpus lexer should require wiring it here.
            preconditionFailure("Unknown swiftLexer key: \(key)")
        }
    }

    private func requireResourceURL(resource: String, ext: String, subdirectory: String? = nil) throws -> URL {
        let candidates: [String?] = {
            if let subdirectory {
                return [subdirectory, "Fixtures/\(subdirectory)"]
            }
            return [nil, "Fixtures"]
        }()

        for candidate in candidates {
            if let url = Bundle.module.url(forResource: resource, withExtension: ext, subdirectory: candidate) {
                return url
            }
        }

        // Fallback: SwiftPM resource paths can vary (especially around top-level directory names).
        // If lookup fails, walk the bundle and find the first matching filename.
        let targetName = "\(resource).\(ext)"
        if let enumerator = FileManager.default.enumerator(at: Bundle.module.bundleURL, includingPropertiesForKeys: nil) {
            for case let url as URL in enumerator {
                if url.lastPathComponent == targetName {
                    return url
                }
            }
        }

        throw XCTSkip(
            "Missing test resource: \(resource).\(ext)" +
                (subdirectory.map { " in \($0)" } ?? "")
        )
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
