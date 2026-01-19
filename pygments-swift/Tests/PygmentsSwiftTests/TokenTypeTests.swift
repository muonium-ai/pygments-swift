import XCTest
@testable import PygmentsSwift

final class TokenTypeTests: XCTestCase {
    func testFromString() {
        XCTAssertEqual(TokenType.fromString(""), .token)
        XCTAssertEqual(TokenType.fromString("Token"), .token)
        XCTAssertEqual(TokenType.fromString("String.Double"), TokenType(["String", "Double"]))
        XCTAssertEqual(TokenType.fromString("Token.Literal.Number"), .number)
    }

    func testSubtype() {
        XCTAssertTrue(TokenType.text.isSubtype(of: .token))
        XCTAssertTrue(TokenType.whitespace.isSubtype(of: .text))
        XCTAssertFalse(TokenType.keyword.isSubtype(of: .string))
    }
}
