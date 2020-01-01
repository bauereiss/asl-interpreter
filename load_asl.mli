(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

val opt_verbose : bool ref

val read_file : string -> bool -> Asl_ast.declaration list
val read_spec : string -> Asl_ast.declaration list
