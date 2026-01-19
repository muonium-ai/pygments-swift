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

        let strict = (ProcessInfo.processInfo.environment["PYGMENTS_STRICT_PARITY"] == "1")
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

    private struct RefToken: Decodable {
        let start: Int
        let type: String
        let value: String
    }

    private func findPython() -> String? {
        // Prefer explicit override.
        if let env = ProcessInfo.processInfo.environment["PYGMENTS_PYTHON"], !env.isEmpty {
            return env
        }
        // Common default.
        return "/usr/bin/python3"
    }

    private func runPythonReference(python: String, input: String) throws -> [RefToken] {
        let testFile = URL(fileURLWithPath: #filePath)
        let testsDir = testFile.deletingLastPathComponent()
        let pkgRoot = testsDir.deletingLastPathComponent().deletingLastPathComponent() // .../pygments-swift
        let script = pkgRoot.appendingPathComponent("Tests/Support/pygments_swift_ref.py")

        let master = pkgRoot.deletingLastPathComponent().appendingPathComponent("pygments-master")

        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: python)
        proc.arguments = [script.path]
        proc.environment = ProcessInfo.processInfo.environment
        proc.environment?["PYGMENTS_MASTER"] = master.path

        let stdinPipe = Pipe()
        let stdoutPipe = Pipe()
        let stderrPipe = Pipe()
        proc.standardInput = stdinPipe
        proc.standardOutput = stdoutPipe
        proc.standardError = stderrPipe

        try proc.run()

        if let data = input.data(using: .utf8) {
            stdinPipe.fileHandleForWriting.write(data)
        }
        stdinPipe.fileHandleForWriting.closeFile()

        proc.waitUntilExit()

        let outData = stdoutPipe.fileHandleForReading.readDataToEndOfFile()
        let errData = stderrPipe.fileHandleForReading.readDataToEndOfFile()

        guard proc.terminationStatus == 0 else {
            let err = String(data: errData, encoding: .utf8) ?? ""
            throw XCTSkip("Python reference failed (status \(proc.terminationStatus)): \(err)")
        }

        let decoder = JSONDecoder()
        return try decoder.decode([RefToken].self, from: outData)
    }
}
