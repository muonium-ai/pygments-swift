import XCTest
@testable import PygmentsSwift

final class CMakeLexerTests: XCTestCase {
    func testCMakeBasics() {
        let lexer = CMakeLexer()
        let input = """
        # comment
        set(N 10)
        set(X ${N})
        message(STATUS "n=${N}")
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Function") && $0.value.lowercased() == "set" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value.contains("${N}") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
