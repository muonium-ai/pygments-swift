import XCTest
@testable import PygmentsSwift

final class VhdlLexerTests: XCTestCase {
    func testVhdlBasics() {
        let lexer = VhdlLexer()
        let input = """
        -- comment
        library ieee;
        use ieee.std_logic_1164.all;

        entity demo is
        end entity;

        architecture rtl of demo is
        begin
        end architecture;
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "entity" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
