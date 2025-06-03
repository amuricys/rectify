namespace Rectify.Json

-- Simple JSON encoding (in real code, use a proper JSON library)
def encodeFloat (f : Float) : String := toString f

def encodeArray (arr : Array (Float × Float × Float)) : String := Id.run do
  let mut result := "["
  for i in [:arr.size] do
    let (t, x, p) := arr[i]!
    if i > 0 then result := result ++ ","
    result := result ++ s!"\{\"t\":{encodeFloat t},\"x\":{encodeFloat x},\"p\":{encodeFloat p}}"
  result := result ++ "]"
  return result

def encodeTrajectory (systemName : String) (params : String) (trajectory : Array (Float × Float × Float)) : String :=
  s!"\{\"system\":\"{systemName}\",\"params\":{params},\"trajectory\":{encodeArray trajectory}}"

end Rectify.Json
