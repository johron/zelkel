from dataclasses import dataclass

@dataclass
class Identifier:
    value: str

@dataclass
class String:
    value: str

@dataclass
class Integer:
    value: int

@dataclass
class LParen:
    pass

@dataclass
class RParen:
    pass

@dataclass
class LBrace:
    pass

@dataclass
class RBrace:
    pass

@dataclass
class Period:
    pass

@dataclass
class Comma:
    pass

@dataclass
class Arrow:
    pass

@dataclass
class Colon:
    pass

@dataclass
class Equals:
    pass

@dataclass
class Plus:
    pass

@dataclass
class Minus:
    pass

@dataclass
class Star:
    pass

@dataclass
class Slash:
    pass

@dataclass
class Ampersand:
    pass

Token = Identifier | String | Integer | LParen | RParen | LBrace | RBrace | Period | Comma | Arrow | Colon | Equals | Plus | Minus | Star | Slash | Ampersand