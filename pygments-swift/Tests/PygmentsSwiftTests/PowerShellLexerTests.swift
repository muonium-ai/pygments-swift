import XCTest
@testable import PygmentsSwift

final class PowerShellLexerTests: XCTestCase {
    func testPowerShellBasics() {
        let lexer = PowerShellLexer()
        let input = """
        # comment
        function Get-Fib([int]$n) {
          if ($n -lt 2) { return $n }
          return (Get-Fib ($n - 1)) + (Get-Fib ($n - 2))
        }
        Write-Output "ok"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value.contains("$n") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
