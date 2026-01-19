import Foundation
import XCTest

struct PythonPygmentsReference {
    struct RefToken: Decodable {
        let start: Int
        let type: String
        let value: String
    }

    static func findPython() -> String? {
        if let env = ProcessInfo.processInfo.environment["PYGMENTS_PYTHON"], !env.isEmpty {
            return env
        }
        return "/usr/bin/python3"
    }

    static func run(input: String, lexerName: String) throws -> [RefToken] {
        guard let python = findPython() else {
            throw XCTSkip("python3 not found; skipping Pygments parity test")
        }

        let testFile = URL(fileURLWithPath: #filePath)
        let supportDir = testFile.deletingLastPathComponent() // .../Support
        let testsDir = supportDir.deletingLastPathComponent() // .../PygmentsSwiftTests
        let pkgRoot = testsDir.deletingLastPathComponent().deletingLastPathComponent() // .../pygments-swift

        let script = pkgRoot.appendingPathComponent("Tests/Support/pygments_swift_ref.py")
        let master = pkgRoot.deletingLastPathComponent().appendingPathComponent("pygments-master")

        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: python)
        proc.arguments = [script.path]
        proc.environment = ProcessInfo.processInfo.environment
        proc.environment?["PYGMENTS_MASTER"] = master.path
        proc.environment?["PYGMENTS_LEXER"] = lexerName

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

        return try JSONDecoder().decode([RefToken].self, from: outData)
    }
}
