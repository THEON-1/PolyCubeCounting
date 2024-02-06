from json import JSONEncoder
from typing import Any
from polycube import PolyCube

class PolyCubeEncdoder(JSONEncoder):
    def default(self, o: Any) -> Any:
        if isinstance(o, PolyCube):
            return list(o.cubes)
        return super().default(o)
