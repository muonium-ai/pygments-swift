import XCTest
@testable import PygmentsSwift

final class Asn1LexerTests: XCTestCase {
    func testAsn1Basics() {
        let lexer = Asn1Lexer()
        let input = #"""
        MyModule DEFINITIONS ::= BEGIN
        Person ::= SEQUENCE {
          name OCTET STRING,
          age INTEGER
        }
        END
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "::=" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
