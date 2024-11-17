import { CustomType as $CustomType } from "../gleam.mjs";
import { type_of, get_symbol } from "../gleam_javascript_ffi.mjs";

export { get_symbol, type_of };

export class UndefinedType extends $CustomType {}

export class ObjectType extends $CustomType {}

export class BooleanType extends $CustomType {}

export class NumberType extends $CustomType {}

export class BigIntType extends $CustomType {}

export class StringType extends $CustomType {}

export class SymbolType extends $CustomType {}

export class FunctionType extends $CustomType {}
