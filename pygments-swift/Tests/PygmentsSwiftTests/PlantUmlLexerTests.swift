import XCTest
@testable import PygmentsSwift

final class PlantUmlLexerTests: XCTestCase {
    func testPlantUmlBasics() {
        let lexer = PlantUmlLexer()
        let input = """
        @startuml
        ' comment
        class Fibonacci
        Fibonacci --> Fibonacci : fib
        @enduml
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.contains("@startuml") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value.contains("-->") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
