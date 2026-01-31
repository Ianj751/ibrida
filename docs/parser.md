# Resources
- https://craftinginterpreters.com/parsing-expressions.html
- https://en.cppreference.com/w/c/language/operator_precedence.html
- // robbed from: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html


# Operator Binding Power
Go In reverse on the cppreference page:
Comma(0, 1), Assignment(2, 3), Logicalor(6, 7) etc.

## AST Example
this is from main1.ibi
```
[src/main.rs:39:5] file_ast = Program {
    declarations: [
        Func(
            FuncDecl {
                name: "is_passing_grade",
                field_list: [
                    Field {
                        name: "score",
                        field_type: Float,
                    },
                ],
                body: BlockStmt {
                    inner: [
                        IfStmt(
                            IfStmt {
                                body: BlockStmt {
                                    inner: [
                                        Return(
                                            ReturnStmt {
                                                expression: Literal(
                                                    "true",
                                                    Bool,
                                                ),
                                                checked_expr_type: Some(
                                                    Bool,
                                                ),
                                            },
                                        ),
                                    ],
                                },
                                condition: BinaryExpr {
                                    op: GreaterEq,
                                    lhs: Var(
                                        "score",
                                        Float,
                                    ),
                                    rhs: Literal(
                                        "60.0",
                                        Float,
                                    ),
                                },
                                else_stmt: Some(
                                    ElseStmt {
                                        body: BlockStmt {
                                            inner: [
                                                Return(
                                                    ReturnStmt {
                                                        expression: Literal(
                                                            "false",
                                                            Bool,
                                                        ),
                                                        checked_expr_type: Some(
                                                            Bool,
                                                        ),
                                                    },
                                                ),
                                            ],
                                        },
                                    },
                                ),
                            },
                        ),
                    ],
                },
                decl_return_type: Bool,
            },
        ),
        Func(
            FuncDecl {
                name: "calculate_average",
                field_list: [
                    Field {
                        name: "a",
                        field_type: Float,
                    },
                    Field {
                        name: "b",
                        field_type: Float,
                    },
                    Field {
                        name: "c",
                        field_type: Float,
                    },
                ],
                body: BlockStmt {
                    inner: [
                        VarDecl(
                            LetStmt {
                                lhs: "sum",
                                declared_type: Float,
                                rhs: BinaryExpr {
                                    op: Addition,
                                    lhs: BinaryExpr {
                                        op: Addition,
                                        lhs: Var(
                                            "a",
                                            Float,
                                        ),
                                        rhs: Var(
                                            "b",
                                            Float,
                                        ),
                                    },
                                    rhs: Var(
                                        "c",
                                        Float,
                                    ),
                                },
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "average",
                                declared_type: Float,
                                rhs: BinaryExpr {
                                    op: Division,
                                    lhs: Var(
                                        "sum",
                                        Float,
                                    ),
                                    rhs: Literal(
                                        "3.0",
                                        Float,
                                    ),
                                },
                            },
                        ),
                        Return(
                            ReturnStmt {
                                expression: Var(
                                    "average",
                                    Float,
                                ),
                                checked_expr_type: Some(
                                    Float,
                                ),
                            },
                        ),
                    ],
                },
                decl_return_type: Float,
            },
        ),
        Func(
            FuncDecl {
                name: "main",
                field_list: [],
                body: BlockStmt {
                    inner: [
                        VarDecl(
                            LetStmt {
                                lhs: "test1",
                                declared_type: Float,
                                rhs: Literal(
                                    "85.5",
                                    Float,
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "test2",
                                declared_type: Float,
                                rhs: Literal(
                                    "92.0",
                                    Float,
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "test3",
                                declared_type: Float,
                                rhs: Literal(
                                    "78.25",
                                    Float,
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "avg",
                                declared_type: Float,
                                rhs: FuncCall(
                                    FuncCall {
                                        id: "calculate_average",
                                        args: [
                                            Var(
                                                "test1",
                                                Float,
                                            ),
                                            Var(
                                                "test2",
                                                Float,
                                            ),
                                            Var(
                                                "test3",
                                                Float,
                                            ),
                                        ],
                                        return_type: Some(
                                            Float,
                                        ),
                                    },
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "passed",
                                declared_type: Bool,
                                rhs: FuncCall(
                                    FuncCall {
                                        id: "is_passing_grade",
                                        args: [
                                            Var(
                                                "avg",
                                                Float,
                                            ),
                                        ],
                                        return_type: Some(
                                            Bool,
                                        ),
                                    },
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "bonus_points",
                                declared_type: Integer,
                                rhs: Literal(
                                    "10",
                                    Integer,
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "base_score",
                                declared_type: Integer,
                                rhs: Literal(
                                    "50",
                                    Integer,
                                ),
                            },
                        ),
                        IfStmt(
                            IfStmt {
                                body: BlockStmt {
                                    inner: [
                                        VarAssign(
                                            AssignStmt {
                                                lhs: "base_score",
                                                rhs: BinaryExpr {
                                                    op: Addition,
                                                    lhs: Var(
                                                        "base_score",
                                                        Integer,
                                                    ),
                                                    rhs: Var(
                                                        "bonus_points",
                                                        Integer,
                                                    ),
                                                },
                                                checked_expr_type: Some(
                                                    Integer,
                                                ),
                                            },
                                        ),
                                    ],
                                },
                                condition: Var(
                                    "passed",
                                    Bool,
                                ),
                                else_stmt: Some(
                                    ElseStmt {
                                        body: BlockStmt {
                                            inner: [
                                                VarAssign(
                                                    AssignStmt {
                                                        lhs: "base_score",
                                                        rhs: BinaryExpr {
                                                            op: Subtraction,
                                                            lhs: Var(
                                                                "base_score",
                                                                Integer,
                                                            ),
                                                            rhs: Literal(
                                                                "5",
                                                                Integer,
                                                            ),
                                                        },
                                                        checked_expr_type: Some(
                                                            Integer,
                                                        ),
                                                    },
                                                ),
                                            ],
                                        },
                                    },
                                ),
                            },
                        ),
                        VarDecl(
                            LetStmt {
                                lhs: "has_extra_credit",
                                declared_type: Bool,
                                rhs: Literal(
                                    "true",
                                    Bool,
                                ),
                            },
                        ),
                        IfStmt(
                            IfStmt {
                                body: BlockStmt {
                                    inner: [
                                        VarAssign(
                                            AssignStmt {
                                                lhs: "base_score",
                                                rhs: BinaryExpr {
                                                    op: Addition,
                                                    lhs: Var(
                                                        "base_score",
                                                        Integer,
                                                    ),
                                                    rhs: Literal(
                                                        "15",
                                                        Integer,
                                                    ),
                                                },
                                                checked_expr_type: Some(
                                                    Integer,
                                                ),
                                            },
                                        ),
                                    ],
                                },
                                condition: Var(
                                    "has_extra_credit",
                                    Bool,
                                ),
                                else_stmt: None,
                            },
                        ),
                        Return(
                            ReturnStmt {
                                expression: Var(
                                    "base_score",
                                    Integer,
                                ),
                                checked_expr_type: Some(
                                    Integer,
                                ),
                            },
                        ),
                    ],
                },
                decl_return_type: Integer,
            },
        ),
    ],
}
```