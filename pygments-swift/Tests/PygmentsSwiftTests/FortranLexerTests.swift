import XCTest
@testable import PygmentsSwift

final class FortranLexerTests: XCTestCase {
    func testFortranBasics() {
        let lexer = FortranLexer()
        let input = """
        ! comment
        program fibdemo
          implicit none
          integer :: n
          n = 10
          print *, 'n=', n
        end program fibdemo
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(500).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "program" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "10" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
