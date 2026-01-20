import XCTest
@testable import PygmentsSwift

final class CobolLexerTests: XCTestCase {
    func testCobolBasics() {
        let lexer = CobolLexer()
        let input = """
              * comment
              IDENTIFICATION DIVISION.
              PROGRAM-ID. FIB.
              PROCEDURE DIVISION.
                  DISPLAY "hello".
                  STOP RUN.
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.uppercased() == "PROGRAM-ID" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
