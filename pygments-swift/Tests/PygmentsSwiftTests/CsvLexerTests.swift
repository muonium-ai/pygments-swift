import XCTest
@testable import PygmentsSwift

final class CsvLexerTests: XCTestCase {
    func testCsvBasics() {
        let lexer = CsvLexer()
        let input = """
        n,fib,comment
        10,55,"quoted, with comma"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .punctuation && $0.value == "," }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "55" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("quoted") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
