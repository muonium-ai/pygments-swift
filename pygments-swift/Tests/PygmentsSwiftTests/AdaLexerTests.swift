import XCTest
@testable import PygmentsSwift

final class AdaLexerTests: XCTestCase {
    func testAdaBasics() {
        let lexer = AdaLexer()
        let input = """
        -- comment
        with Ada.Text_IO;
        procedure Fib is
        begin
            Ada.Text_IO.Put_Line("hi");
        end Fib;
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "procedure" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
