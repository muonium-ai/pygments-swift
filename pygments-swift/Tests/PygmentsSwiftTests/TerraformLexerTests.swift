import XCTest
@testable import PygmentsSwift

final class TerraformLexerTests: XCTestCase {
    func testTerraformBasics() {
        let lexer = TerraformLexer()
        let input = """
        # comment
        variable "n" { default = 10 }
        output "fib" { value = "${var.n}" }
        resource "null_resource" "example" {}
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "resource" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("${var.n}") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
