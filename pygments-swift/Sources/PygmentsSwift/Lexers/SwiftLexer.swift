import Foundation

/// Swift lexer based on Pygments' `SwiftLexer` (ported from `pygments.lexers.objective`).
public final class SwiftLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        // Swift identifiers are Unicode; use Unicode identifier properties.
        // ICU (used by NSRegularExpression) supports XID_Start/XID_Continue.
        let identPattern = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        let keywordPattern = RegexHelpers.words([
            "as", "async", "await", "break", "case", "catch", "continue", "default", "defer",
            "do", "else", "fallthrough", "for", "guard", "if", "in", "is",
            "repeat", "return", "#selector", "switch", "throw", "try",
            "where", "while"
        ], suffix: "\\b")

        let reservedPattern = RegexHelpers.words([
            "associativity", "convenience", "dynamic", "didSet", "final",
            "get", "indirect", "infix", "inout", "lazy", "left", "mutating",
            "none", "nonmutating", "optional", "override", "postfix",
            "precedence", "prefix", "Protocol", "required", "rethrows",
            "right", "set", "throws", "Type", "unowned", "weak", "willSet",
            "@availability", "@autoclosure", "@noreturn",
            "@NSApplicationMain", "@NSCopying", "@NSManaged", "@objc",
            "@UIApplicationMain", "@IBAction", "@IBDesignable",
            "@IBInspectable", "@IBOutlet"
        ], suffix: "\\b")

        let declPattern = RegexHelpers.words([
            "actor", "associatedtype", "class", "deinit", "enum", "extension", "func", "import",
            "init", "internal", "let", "operator", "private", "protocol", "public",
            "static", "struct", "subscript", "typealias", "var"
        ], suffix: "\\b")

        let builtinTypesPattern = RegexHelpers.words([
            // Mirrors Pygments' SwiftLexer global types list.
            "Array", "AutoreleasingUnsafeMutablePointer", "BidirectionalReverseView",
            "Bit", "Bool", "CFunctionPointer", "COpaquePointer", "CVaListPointer",
            "Character", "ClosedInterval", "CollectionOfOne", "ContiguousArray",
            "Dictionary", "DictionaryGenerator", "DictionaryIndex", "Double",
            "EmptyCollection", "EmptyGenerator", "EnumerateGenerator",
            "EnumerateSequence", "FilterCollectionView",
            "FilterCollectionViewIndex", "FilterGenerator", "FilterSequenceView",
            "Float", "Float80", "FloatingPointClassification", "GeneratorOf",
            "GeneratorOfOne", "GeneratorSequence", "HalfOpenInterval", "HeapBuffer",
            "HeapBufferStorage", "ImplicitlyUnwrappedOptional", "IndexingGenerator",
            "Int", "Int16", "Int32", "Int64", "Int8", "LazyBidirectionalCollection",
            "LazyForwardCollection", "LazyRandomAccessCollection",
            "LazySequence", "MapCollectionView", "MapSequenceGenerator",
            "MapSequenceView", "MirrorDisposition", "ObjectIdentifier", "OnHeap",
            "Optional", "PermutationGenerator", "QuickLookObject",
            "RandomAccessReverseView", "Range", "RangeGenerator", "RawByte", "Repeat",
            "ReverseBidirectionalIndex", "ReverseRandomAccessIndex", "SequenceOf",
            "SinkOf", "Slice", "StaticString", "StrideThrough", "StrideThroughGenerator",
            "StrideTo", "StrideToGenerator", "String", "UInt", "UInt16", "UInt32",
            "UInt64", "UInt8", "UTF16", "UTF32", "UTF8", "UnicodeDecodingResult",
            "UnicodeScalar", "Unmanaged", "UnsafeBufferPointer",
            "UnsafeBufferPointerGenerator", "UnsafeMutableBufferPointer",
            "UnsafeMutablePointer", "UnsafePointer", "Zip2", "ZipGenerator2",
            // Protocols
            "AbsoluteValuable", "AnyObject", "ArrayLiteralConvertible",
            "BidirectionalIndexType", "BitwiseOperationsType",
            "BooleanLiteralConvertible", "BooleanType", "CVarArgType",
            "CollectionType", "Comparable", "DebugPrintable",
            "DictionaryLiteralConvertible", "Equatable",
            "ExtendedGraphemeClusterLiteralConvertible",
            "ExtensibleCollectionType", "FloatLiteralConvertible",
            "FloatingPointType", "ForwardIndexType", "GeneratorType", "Hashable",
            "IntegerArithmeticType", "IntegerLiteralConvertible", "IntegerType",
            "IntervalType", "MirrorType", "MutableCollectionType", "MutableSliceable",
            "NilLiteralConvertible", "OutputStreamType", "Printable",
            "RandomAccessIndexType", "RangeReplaceableCollectionType",
            "RawOptionSetType", "RawRepresentable", "Reflectable", "SequenceType",
            "SignedIntegerType", "SignedNumberType", "SinkType", "Sliceable",
            "Streamable", "Strideable", "StringInterpolationConvertible",
            "StringLiteralConvertible", "UnicodeCodecType",
            "UnicodeScalarLiteralConvertible", "UnsignedIntegerType",
            "_ArrayBufferType", "_BidirectionalIndexType", "_CocoaStringType",
            "_CollectionType", "_Comparable", "_ExtensibleCollectionType",
            "_ForwardIndexType", "_Incrementable", "_IntegerArithmeticType",
            "_IntegerType", "_ObjectiveCBridgeable", "_RandomAccessIndexType",
            "_RawOptionSetType", "_SequenceType", "_Sequence_Type",
            "_SignedIntegerType", "_SignedNumberType", "_Sliceable", "_Strideable",
            "_SwiftNSArrayRequiredOverridesType", "_SwiftNSArrayType",
            "_SwiftNSCopyingType", "_SwiftNSDictionaryRequiredOverridesType",
            "_SwiftNSDictionaryType", "_SwiftNSEnumeratorType",
            "_SwiftNSFastEnumerationType", "_SwiftNSStringRequiredOverridesType",
            "_SwiftNSStringType", "_UnsignedIntegerType",
            // Variables
            "C_ARGC", "C_ARGV", "Process",
            // Typealiases
            "Any", "AnyClass", "BooleanLiteralType", "CBool", "CChar", "CChar16",
            "CChar32", "CDouble", "CFloat", "CInt", "CLong", "CLongLong", "CShort",
            "CSignedChar", "CUnsignedInt", "CUnsignedLong", "CUnsignedShort",
            "CWideChar", "ExtendedGraphemeClusterType", "Float32", "Float64",
            "FloatLiteralType", "IntMax", "IntegerLiteralType", "StringLiteralType",
            "UIntMax", "UWord", "UnicodeScalarType", "Void", "Word",
            // Foundation/Cocoa
            "NSErrorPointer", "NSObjectProtocol", "Selector"
        ], suffix: "\\b")

        let builtinFunctionsPattern = RegexHelpers.words([
            "abs", "advance", "alignof", "alignofValue", "assert", "assertionFailure",
            "contains", "count", "countElements", "debugPrint", "debugPrintln",
            "distance", "dropFirst", "dropLast", "dump", "enumerate", "equal",
            "extend", "fatalError", "filter", "find", "first", "getVaList", "indices",
            "insert", "isEmpty", "join", "last", "lazy", "lexicographicalCompare",
            "map", "max", "maxElement", "min", "minElement", "numericCast", "overlaps",
            "partition", "precondition", "preconditionFailure", "prefix", "print",
            "println", "reduce", "reflect", "removeAll", "removeAtIndex", "removeLast",
            "removeRange", "reverse", "sizeof", "sizeofValue", "sort", "sorted",
            "splice", "split", "startsWith", "stride", "strideof", "strideofValue",
            "suffix", "swap", "toDebugString", "toString", "transcode",
            "underestimateCount", "unsafeAddressOf", "unsafeBitCast", "unsafeDowncast",
            "withExtendedLifetime", "withUnsafeMutablePointer",
            "withUnsafeMutablePointers", "withUnsafePointer", "withUnsafePointers",
            "withVaList"
        ], suffix: "\\b")

        return [
            "root": [
                // Whitespace and Comments
                .rule(Rule("\\n", action: .token(.text))),
                .rule(Rule("\\s+", action: .token(.whitespace))),
                .rule(Rule("//", action: .token(.comment.child("Single")), newState: .ops([.push("comment-single")]))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment-multi")]))),
                .rule(Rule("#(if|elseif|else|endif|available)\\b", action: .token(.comment.child("Preproc")), newState: .ops([.push("preproc")]))),

                .include("keywords"),

                // Global Types (minimal subset)
                .rule(Rule(builtinTypesPattern, action: .token(.name.child("Builtin")))),

                // Functions
                .rule(Rule(builtinFunctionsPattern, action: .token(.name.child("Builtin").child("Pseudo")))),

                // Implicit Block Variables
                .rule(Rule("\\$\\d+", action: .token(.name.child("Variable")))),

                // Numeric literals
                .rule(Rule("0b[01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0o[0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("0x[0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[0-9][0-9_]*(\\.[0-9_]+[eE][+\\-]?[0-9_]+|\\.[0-9_]*|[eE][+\\-]?[0-9_]+)", action: .token(.number.child("Float")))),
                .rule(Rule("[0-9][0-9_]*", action: .token(.number.child("Integer")))),

                // String literals
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.push("string-multi")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("string")]))),

                // Operators and punctuation
                .rule(Rule("[(){}\\[\\].,:;=@#`?]|->|[<&?](?=\\w)|(?<=\\w)[>!?]", action: .token(.punctuation))),
                .rule(Rule("[/=\\-+!*%<>&|^?~]+", action: .token(.operator))),

                // Identifier
                .rule(Rule(identPattern, action: .token(.name)))
            ],

            "keywords": [
                .rule(Rule(keywordPattern, action: .token(.keyword))),
                .rule(Rule("@availability\\([^)]+\\)", action: .token(.keyword.child("Reserved")))),
                .rule(Rule(reservedPattern, action: .token(.keyword.child("Reserved")))),
                .rule(Rule("(as|dynamicType|false|is|nil|self|Self|super|true|__COLUMN__|__FILE__|__FUNCTION__|__LINE__|_|#(?:file|line|column|function))\\b", action: .token(.keyword.child("Constant")))),
                .rule(Rule("import\\b", action: .token(.keyword.child("Declaration")), newState: .ops([.push("module")]))),
                .rule(Rule("(class|enum|extension|struct|protocol)(\\s+)(" + identPattern + ")", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Class")]))),
                .rule(Rule("(func)(\\s+)(" + identPattern + ")", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Function")]))),
                .rule(Rule("(var|let)(\\s+)(" + identPattern + ")", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Variable")]))),
                .rule(Rule(declPattern, action: .token(.keyword.child("Declaration"))))
            ],

            "comment": [
                .rule(Rule(":param: " + identPattern + "|:returns?:|(FIXME|MARK|TODO):", action: .token(.comment.child("Special"))))
            ],

            "comment-single": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .include("comment"),
                .rule(Rule("[^\\n]+", action: .token(.comment.child("Single"))))
            ],

            "comment-multi": [
                .include("comment"),
                .rule(Rule("[^*/]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.pushCurrent]))),
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[*/]+", action: .token(.comment.child("Multiline"))))
            ],

            "module": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .rule(Rule(identPattern, action: .token(.name.child("Class")))),
                .include("root")
            ],

            "preproc": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .include("keywords"),
                .rule(Rule(identPattern, action: .token(.comment.child("Preproc")))),
                .include("root")
            ],

            "string": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .include("string-common")
            ],

            "string-multi": [
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.pop]))),
                .include("string-common")
            ],

            "string-common": [
                .rule(Rule("\\\\\\(", action: .token(.string.child("Interpol")), newState: .ops([.push("string-intp")]))),
                .rule(Rule(#"\\['"\\nrt]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}"#, action: .token(.string.child("Escape")))),
                .rule(Rule("[^\\\\\"]+", action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string)))
            ],

            "string-intp": [
                .rule(Rule("\\(", action: .token(.string.child("Interpol")), newState: .ops([.pushCurrent]))),
                .rule(Rule("\\)", action: .token(.string.child("Interpol")), newState: .ops([.pop]))),
                .include("root")
            ]
        ]
    }
}
