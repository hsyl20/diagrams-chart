import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Chart.Bar

values :: [(String,Double)]
values = [
   ("Apple", 10.0),
   ("Orange", 20.0),
   ("Lemon", 25.5),
   ("Peach", 14.2)
  ]

main :: IO ()
main = defaultMain $ plotBars Horizontal values
