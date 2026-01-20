import XCTest
@testable import PygmentsSwift

final class SmithyLexerTests: XCTestCase {
    func testSmithyBasics() {
        let lexer = SmithyLexer()
        let input = #"""
        $version: "2"
        namespace example.weather

        @readonly
        operation GetWeather {
          input: GetWeatherInput
          output: GetWeatherOutput
        }
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "namespace" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "readonly" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
