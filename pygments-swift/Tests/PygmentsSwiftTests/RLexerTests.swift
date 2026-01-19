import XCTest
@testable import PygmentsSwift

final class RLexerTests: XCTestCase {
    func testRLexingBasics() {
        let lexer = RLexer()
        let input = """
        # comment
        f <- function(x) {
          y <- x + 1
          if (is.na(y)) return(NA)
          z <- y %>% 2
          s <- "hi\n"
          t <- 'ok'
          `weird name` <- TRUE
          return(z)
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .operator) && $0.value.contains("<-") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
