namespace JsonPlugin

open System
open Myriad.Core
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler
open FsAst

type CompileTimeJsonEncodingAttribute() = inherit Attribute()
[<AutoOpen>]
module Util =
    let getAttributeName (attr: SynAttribute) = attr.TypeName.AsString
    let yes = true
    let no = false

type WhatIsThat =
    | ItsAnUnion of moduleIdent: LongIdent * typeIdent: LongIdent * unionCases: SynUnionCases
    | ItsAnRecord of moduleIdent: LongIdent * typeIdent: LongIdent * recordFields: SynFields

[<MyriadGenerator("example1")>]
type Example1Gen() =
    let mkUncurriedSimpleExprCall objectIdent expr =
        SynExpr.CreateApp(
            SynExpr.CreateLongIdent(
                false,
                objectIdent,
                None
            ), SynExpr.CreateParen(expr))
    let mkUncurriedSimpleCall objectIdent arguments =
        SynExpr.CreateApp(
            SynExpr.CreateLongIdent(
                false,
                objectIdent,
                None
            ),
            match arguments with
            | [] -> SynExpr.CreateConst SynConst.Unit
            | _ ->
                SynExpr.CreateParen(
                    match arguments with
                    | [ arg ] ->
                        SynExpr.CreateLongIdent(
                            false,
                            arg,
                            None
                        )
                    | _ ->
                        arguments
                        |> List.map(fun arg -> 
                            SynExpr.CreateLongIdent(
                                false,
                                arg,
                                None
                            ))
                        |> SynExpr.CreateTuple
                )
        )
    let mkEncoder members =
        SynModuleDecl.CreateType([Ident.Create "Encoder"] |> SynComponentInfoRcd.Create, members)
    //let mkMemberBinding name parameters expr =
    //    SynBinding.Binding(
    //        SynAccess.Public |> Some,
    //        SynBindingKind.NormalBinding,
    //        no,
    //        no,
    //        [],
    //        PreXmlDoc.Empty,
    //        SynValData(Some {
    //            IsInstance = false
    //            IsDispatchSlot = false
    //            IsOverrideOrExplicitImpl = false
    //            IsFinal = false
    //            MemberKind = MemberKind.Member },
    //            SynValInfo(
    //                [parameters |> List.map(fun (paramName, _) -> SynArgInfo ([],false, Ident.Create paramName |> Some))],
    //                 SynArgInfo ([],false,None)),
    //            None),
    //        SynPat.LongIdent(
    //            name,
    //            None, None,
    //            SynConstructorArgs.Pats([
    //                SynPat.Paren(
    //                    SynPat.Typed(
    //                        SynPat.Named(
    //                            SynPat.Wild(Range.rangeStartup),
    //                            Ident.Create paramName, false, None, Range.rangeStartup
    //                        ),
    //                        SynType.LongIdent(LongIdentWithDots.CreateString typ),
    //                        Range.rangeStartup
    //                    ),
    //                    Range.rangeStartup
    //                )
    //            ]),
    //            None, Range.rangeStartup
    //        ),
    //        None,
    //        expr,
    //        Range.rangeStartup,
    //        NoSequencePointAtInvisibleBinding
    //    )
    let mkEncodeMemberBindingSpecialType paramName typ expr =
        SynBinding.Binding(
            None,
            SynBindingKind.NormalBinding,
            yes,
            no,
            [],
            PreXmlDoc.Empty,
            SynValData(Some {
                IsInstance = false
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.Member },
                SynValInfo(
                    [[SynArgInfo ([],false, Ident.Create paramName |> Some)]],
                     SynArgInfo ([],false,None)),
                None),
            SynPat.LongIdent(
                LongIdentWithDots.CreateString "Encode",
                None, None,
                SynConstructorArgs.Pats([
                    SynPat.Paren(
                        SynPat.Typed(
                            SynPat.Named(
                                SynPat.Wild(Range.rangeStartup),
                                Ident.Create paramName, false, None, Range.rangeStartup
                            ),
                            typ,
                            Range.rangeStartup
                        ),
                        Range.rangeStartup
                    )
                ]),
                None, Range.rangeStartup
            ),
            None,
            expr,
            Range.rangeStartup,
            NoSequencePointAtInvisibleBinding
        )
    let mkEncodeMemberBinding paramName typ expr =
        SynBinding.Binding(
            None,
            SynBindingKind.NormalBinding,
            yes,
            no,
            [],
            PreXmlDoc.Empty,
            SynValData(Some {
                IsInstance = false
                IsDispatchSlot = false
                IsOverrideOrExplicitImpl = false
                IsFinal = false
                MemberKind = MemberKind.Member },
                SynValInfo(
                    [[SynArgInfo ([],false, Ident.Create paramName |> Some)]],
                     SynArgInfo ([],false,None)),
                None),
            SynPat.LongIdent(
                LongIdentWithDots.CreateString "Encode",
                None, None,
                SynConstructorArgs.Pats([
                    SynPat.Paren(
                        SynPat.Typed(
                            SynPat.Named(
                                SynPat.Wild(Range.rangeStartup),
                                Ident.Create paramName, false, None, Range.rangeStartup
                            ),
                            SynType.LongIdent(LongIdentWithDots.CreateString typ),
                            Range.rangeStartup
                        ),
                        Range.rangeStartup
                    )
                ]),
                None, Range.rangeStartup
            ),
            None,
            expr,
            Range.rangeStartup,
            NoSequencePointAtInvisibleBinding
        )

    let mkFullMemberFromBinding binding = SynMemberDefn.Member(binding, Range.rangeStartup)
    
    let primitiveTypesDeserialization =
        let numberIdent = LongIdentWithDots.CreateString "number" |> List.singleton
        seq {
            "int16", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Int16.Parse") numberIdent
            "int32", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Int32.Parse") numberIdent
            "int64", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Int64.Parse") numberIdent
            "uint16", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Uint16.Parse") numberIdent
            "uint32", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Uint32.Parse") numberIdent
            "uint64", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Uint64.Parse") numberIdent
            "byte", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Byte.Parse") numberIdent
            "sbyte", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Sbyte.Parse") numberIdent
            "float", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Float.Parse") numberIdent
            "float32", mkUncurriedSimpleCall (LongIdentWithDots.CreateString "System.Single.Parse") numberIdent
        }
        |> Seq.map(fun (typeName, expr) ->
            expr
            |> mkEncodeMemberBinding "number" typeName
            |> mkFullMemberFromBinding)

    let primitiveTypesEncoding =
        let callToString = mkUncurriedSimpleCall (LongIdentWithDots.CreateString "number.ToString") []
        seq {
            "int16"
            "int32"
            "int64"
            "uint16"
            "uint32"
            "uint64"
            "float"
            "float32"
            "byte"
            "sbyte"
        }
        |> Seq.map(fun typeName ->
            Ident.Create "number"
            |> SynExpr.CreateIdent
            |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "decimal")
            |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.Number")
            |> mkEncodeMemberBinding "number" typeName
            |> mkFullMemberFromBinding)

    let stringEncoder =
        let name = "str"
        let typ = "string"
        Ident.Create name
        |> SynExpr.CreateIdent
        |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.String")
        |> mkEncodeMemberBinding name typ
        |> mkFullMemberFromBinding

    let arrayEncoder =
        let name = "list"
        let typ =
            SynType.HashConstraint(
                SynType.App(
                    SynType.CreateLongIdent "array",
                    None,
                    [SynType.Var(
                        SynTypar.Typar(Ident.Create "a", TyparStaticReq.NoStaticReq, false),
                        Range.rangeStartup
                    )],
                    [],
                    None,
                    false,
                    Range.rangeStartup
                ),
                Range.rangeStartup
            )
            // "#seq<'a>"
        let expr =
            SynExpr.App
                (ExprAtomicFlag.NonAtomic,false,
                 SynExpr.App
                   (ExprAtomicFlag.NonAtomic,true, SynExpr.CreateIdentString "op_PipeRight", SynExpr.CreateIdentString "list",
                    Range.rangeStartup),
                 SynExpr.App
                   (ExprAtomicFlag.Atomic,false,
                    SynExpr.LongIdent
                      (false,
                       LongIdentWithDots
                         ([Ident.Create "Seq"; Ident.Create "map" ],[Range.rangeStartup]),
                       None,Range.rangeStartup),
                    SynExpr.Paren
                      (SynExpr.Lambda
                         (false,false,
                          SynSimplePats.SimplePats
                            ([SynSimplePat.Id
                                (Ident.Create "element",None,false,false,false,
                                 Range.rangeStartup)],
                             Range.rangeStartup),
                          SynExpr.App
                            (ExprAtomicFlag.NonAtomic,false,
                             SynExpr.LongIdent
                               (false,
                                LongIdentWithDots
                                  ([Ident.Create "Encoder"; Ident.Create "Encode" ],
                                   [Range.rangeStartup]),
                                None,Range.rangeStartup),
                             SynExpr.CreateIdentString "element",
                             Range.rangeStartup),
                          Range.rangeStartup),
                       Range.rangeStartup,
                       Some Range.rangeStartup,
                       Range.rangeStartup),
                    Range.rangeStartup),
                 Range.rangeStartup)
        
        expr
        |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "Seq.toArray")
        |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.Array")
        |> mkEncodeMemberBindingSpecialType name typ
        |> mkFullMemberFromBinding


    let boolEncoder =
        let name = "value"
        let typ = "bool"
        Ident.Create name
        |> SynExpr.CreateIdent
        |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.Boolean")
        |> mkEncodeMemberBinding name typ
        |> mkFullMemberFromBinding

    let defaultEncoders =
        primitiveTypesEncoding
        |> Seq.append [
            stringEncoder
            boolEncoder
            arrayEncoder
        ]

    let mkEncodeMemberBindingFromRecordField (moduleIdent: LongIdent) typeIdent recordFields =
        let currObjectTypeName = typeIdent |> List.append moduleIdent |> List.map(fun ident -> ident.idText) |> String.concat "."
        let currObjectName = typeIdent |> List.last |> (fun last -> last.idText.ToLower())
        let fields =
            seq {
                for Field(_, _, fieldIdentOption, synType, _, _, _, _fieldRange) in recordFields do
                    match fieldIdentOption with
                    | None -> ()
                    | Some fieldIdent ->
                        match synType with
                        | SynType.LongIdent typeIdent -> yield (fieldIdent.idText, typeIdent.AsString)
                        | _ -> ()
            }
        let expr =
            let fieldEncoders =
                [| for (fieldName, _) in fields ->
                    SynExpr.Tuple([
                        SynConst.CreateString fieldName
                        |> SynExpr.CreateConst
                        SynExpr.CreateApp(
                            SynExpr.CreateLongIdent(
                                false,
                                LongIdentWithDots.CreateString "Encoder.Encode",
                                None
                            ),
                            SynExpr.CreateParen(
                                SynExpr.CreateLongIdent(
                                    false,
                                    LongIdentWithDots.Create [ currObjectName; fieldName ],
                                    None
                                )
                            )
                        )
                    ], [], Range.rangeStartup) |]
                |> Array.reduceBack(fun expr1 expr2 ->
                    SynExpr.Sequential(
                        SequencePointsAtSeq, true,
                        expr1,
                        expr2,
                        Range.rangeStartup
                    )
                )
            SynExpr.ArrayOrListOfSeqExpr(
                true,
                SynExpr.CompExpr(
                    true,
                    ref true,
                    fieldEncoders,
                    Range.rangeStartup
                ),
                Range.rangeStartup
            )
        expr
        |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.Record")
        |> mkEncodeMemberBinding currObjectName currObjectTypeName
        |> mkFullMemberFromBinding

    let mkEncodeMemberBindingFromUnionDecl (moduleIdent: LongIdent) typeIdent  (cases: SynUnionCases) =
        let currObjectTypeName = typeIdent |> List.append moduleIdent |> List.map(fun ident -> ident.idText) |> String.concat "."
        let currObjectName = typeIdent |> List.last |> (fun last -> last.idText.ToLower())
        let caseEncoders =
            let synPatUnionCaseFields (fields: SynFields) =
                match fields with
                | [] -> SynPat.Wild(Range.rangeStartup)
                | _ ->
                    SynPat.Tuple(
                        [
                            for field in fields ->
                                SynPat.Named(
                                    SynPat.Wild(Range.rangeStartup),
                                    field.ToRcd.Id |> Option.get, // Save to call because it's checked before call
                                    false,
                                    None,
                                    Range.rangeStartup
                                )
                        ],
                        Range.rangeStartup
                    )
            let encodeUnionCase (caseIdent: Ident) (fields: SynFields) =
                let expr =
                    match fields with
                    | [] -> SynExpr.ArrayOrList(true, [], Range.rangeStartup)
                    | _ ->
                        let fieldEncoders =
                            [| for field in fields ->
                                let fieldName = field.ToRcd.Id |> Option.get in
                                SynExpr.Tuple([
                                    SynConst.CreateString fieldName.idText |> SynExpr.CreateConst
                                    SynExpr.CreateApp(
                                        SynExpr.CreateLongIdent(
                                            false,
                                            LongIdentWithDots.CreateString "Encoder.Encode",
                                            None
                                        ),
                                        SynExpr.CreateParen(
                                            SynExpr.CreateLongIdent(
                                                false,
                                                LongIdentWithDots([fieldName], []), // Save to call because it's checked before call
                                                None
                                            )
                                        )
                                    )
                                ], [], Range.rangeStartup) |]
                            |> Array.reduceBack(fun expr1 expr2 ->
                                SynExpr.Sequential(
                                    SequencePointsAtSeq, true,
                                    expr1,
                                    expr2,
                                    Range.rangeStartup
                                )
                            )
                        SynExpr.ArrayOrListOfSeqExpr(
                            true,
                            SynExpr.CompExpr(
                                true,
                                ref true,
                                fieldEncoders,
                                Range.rangeStartup
                            ),
                            Range.rangeStartup
                        )
                    |> mkUncurriedSimpleExprCall (LongIdentWithDots.CreateString "JsonValue.Record")
                SynExpr.Tuple([
                    SynExpr.CreateConstString (caseIdent.idText)
                    expr
                ], [], Range.rangeStartup)
            seq {
                for UnionCase(_attrs, ident, caseType, _, _, range) in cases do
                    let caseName = ident
                    match caseType with
                    | SynUnionCaseType.UnionCaseFullType _ -> failwithf "Unsupported union case %A of union %A" ident currObjectTypeName
                    | SynUnionCaseType.UnionCaseFields fields ->
                        if fields |> List.exists(fun field -> field.ToRcd.Id |> Option.isNone) then
                            failwithf "All fields of case %A from union %A need to have a identifier!" caseName currObjectTypeName
                        else
                            SynMatchClause.Clause(SynPat.LongIdent
                                  (LongIdentWithDots ([caseName] |> List.append typeIdent |> List.append moduleIdent,[]),
                                   None,None,
                                   Pats
                                     [ synPatUnionCaseFields fields ],
                                   None,
                                   Range.rangeStartup),
                                None,
                                encodeUnionCase ident fields,
                                Range.rangeStartup,
                                SequencePointAtTarget)
            }


        SynExpr.App
            (ExprAtomicFlag.NonAtomic,
            false,
            SynExpr.CreateLongIdent(false, 
                LongIdentWithDots.CreateString "JsonValue.Record",
                None),
            SynExpr.ArrayOrListOfSeqExpr
                (true,
                SynExpr.CompExpr
                   (true,
                    ref true,
                    SynExpr.Match
                       (Range.rangeStartup |> SequencePointAtBinding,
                        Ident.Create currObjectName |> SynExpr.Ident,
                        caseEncoders |> Seq.toList,
                        false,
                        Range.rangeStartup),
                    Range.rangeStartup
                ),
                Range.rangeStartup
                ),
            Range.rangeStartup
            )
        |> mkEncodeMemberBinding currObjectName currObjectTypeName
        |> mkFullMemberFromBinding

    let openJsonPluginNamespace =
        LongIdentWithDots.CreateString "JsonPlugin.Types"
        |> SynModuleDecl.CreateOpen

    interface IMyriadGenerator with
        member __.Generate(namespace', _ast) =
            printfn "Hi"
            let relevantDecls =
                seq {
                    match _ast with
                    | ParsedInput.ImplFile(ParsedImplFileInput(fileName, isScript, _, _, _, modules, _)) ->
                        for SynModuleOrNamespace(moduleIdent, _, _, decls, _, _, _, range) in modules do
                            for decl in decls do
                                match decl with
                                | SynModuleDecl.Types(types, _) ->
                                    for SynTypeDefn.TypeDefn(componentInfo, typeDefnRepr, _members, _) in types do
                                        let (ComponentInfo(typeAttributes, _typeParams, _constraints, typeIdent, _, _, _, _)) = componentInfo
                                        printfn "%A" typeAttributes
                                        if typeAttributes |> List.exists(fun attr -> (Util.getAttributeName attr) = (typeof<CompileTimeJsonEncodingAttribute>.FullName.Replace("Attribute", ""))) then
                                            match typeDefnRepr with
                                            | SynTypeDefnRepr.Simple(repr, _) ->
                                                match repr with
                                                | SynTypeDefnSimpleRepr.Union(_, cases, _) ->
                                                    yield WhatIsThat.ItsAnUnion(moduleIdent, typeIdent, cases)
                                                | SynTypeDefnSimpleRepr.Record(_, recordFields, recordDefinitionRange) ->
                                                    yield WhatIsThat.ItsAnRecord(moduleIdent, typeIdent, recordFields)
                                                | _ -> ()
                                            | _ -> ()
                                | _ -> ()
                    | _ -> ()
                }
            let serializer =
                relevantDecls
                |> Seq.map(function
                     | WhatIsThat.ItsAnRecord(moduleIdent, typeIdent, recordFields) ->
                        mkEncodeMemberBindingFromRecordField  moduleIdent typeIdent recordFields
                     | WhatIsThat.ItsAnUnion(moduleIdent, typeIdent, cases) ->
                        mkEncodeMemberBindingFromUnionDecl moduleIdent typeIdent cases
                )
                |> Seq.append defaultEncoders
                |> Seq.toList |> mkEncoder

            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with Declarations = [
                        openJsonPluginNamespace
                        serializer ]
                }

            namespaceOrModule