import Html exposing (Html)
import Html.Attributes
import Char
import Array
import Maybe
import Time exposing (Time)
import Random
import List exposing (map)
import Tuple exposing (first, second)

giBit amount =
  (*) (2 ^ amount)

arrayGetCyclic array withDefault index=
  Array.get (index % (Array.length array)) array |> Maybe.withDefault withDefault

giMode charset =
  arrayGetCyclic charset ' ' >> Char.toCode

gsAll gFunc =
  String.map (\n -> if n == ' ' then ' ' else (Char.toCode >> gFunc >> Char.fromCode) n)

gsEnd gFunc index text =
  (String.left index text) ++ ((String.dropLeft index text) |> gsAll gFunc)

originalGlitch = Array.fromList -- <| String.toList "◆▒␉␌␍␊°±␤␋┘┐┌└┼⎺⎻─⎼⎽├┤┴┬│"
               <| map Char.fromCode [9670, 9618, 9225, 9228, 9229, 9226, 176, 177, 9252, 9227, 9496, 9488, 9484, 9492, 9532, 9146, 9147, 9472, 9148, 9149, 9500, 9508, 9524, 9516, 9474]

boxGlitch = Array.fromList --<| String.toList "─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿"
          <| map Char.fromCode [9472, 9473, 9474, 9475, 9476, 9477, 9478, 9479, 9480, 9481, 9482, 9483, 9484, 9485, 9486, 9487, 9488, 9489, 9490, 9491, 9492, 9493, 9494, 9495, 9496, 9497, 9498, 9499, 9500, 9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, 9520, 9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528, 9529, 9530, 9531, 9532, 9533, 9534, 9535, 9536, 9537, 9538, 9539, 9540, 9541, 9542, 9543, 9544, 9545, 9546, 9547, 9548, 9549, 9550, 9551, 9552, 9553, 9554, 9555, 9556, 9557, 9558, 9559, 9560, 9561, 9562, 9563, 9564, 9565, 9566, 9567, 9568, 9569, 9570, 9571, 9572, 9573, 9574, 9575, 9576, 9577, 9578, 9579, 9580, 9581, 9582, 9583, 9584, 9585, 9586, 9587, 9588, 9589, 9590, 9591, 9592, 9593, 9594, 9595, 9596, 9597, 9598, 9599]

prefix = "I hope "

phraseFirst = "..."

randFunc num =
  Random.step (Random.float 0 1) (Random.initialSeed num) |> first

shuffle list seed =
  List.map2 (\index item -> (item, randFunc (11*index + 34*seed))) (List.range 0 ((List.length list) - 1)) list
  |> List.sortBy second |> map first

phrases = [
  "someone can hear me",
  "someone is listening",
  "someone is out there",
  "someone believes it too",
  "someone feels what I feel",
  "someone is there for me",
  "I find someone",
  "she believes me",
  "they believe me",
  "he believes me",
  "she trusts me",
  "they trust me",
  "he trusts me",
  "she cares for me",
  "they care for me",
  "he cares for me",
  "she supports me",
  "they support me",
  "he supports me",
  "she understands",
  "they understand",
  "he understands",
  "she accepts who I am",
  "they accept who I am",
  "he accepts who I am",
  "you believe in me",
  "you believe me",
  "you feel what I feel",
  "you're there for me",
  "you accept who I am",
  "you understand",
  "you know what it's like",
  "you know how I feel",
  "you can make it",
  "you can help me",
  "you're open about it",
  "you're all right with it",
  "I can see you again",
  "I can meet you again",
  "you come around again",
  "we see each other soon",
  "we see each other again",
  "we meet again",
  "we do this again",
  "we keep in touch",
  "you'll keep in touch",
  "we make this a thing",
  "I'll see you soon",
  "you'll see me again",
  "you'll be back soon",
  "you'll return someday",
  "I'll be back soon",
  "I'll be return someday",
  "I'll be there for you",
  "I accept who I am",
  "I stay true to myself",
  "I've thought it through",
  "I know where I stand",
  "I know where I'm going",
  "I know what I want",
  "I know what I stand for",
  "I know what I want to do",
  "I find what I love",
  "I'll make it",
  "I find a way",
  "I remain hopeful",
  "I'm not alone",
  "I can be heard",
  "I'm among friends",
  "I can be helpful",
  "I'm making a difference",
  "I'm helping someone",
  "I'm making it better",
  "it's meaningful to someone",
  "it continues after I leave",
  "it's worth it",
  "someone finds it useful",
  "it's just a misunderstanding",
  "it's just a mistake",
  "I'll be out of here",
  "I'm not stuck here",
  "it's the last time",
  "this is it",
  "there's more",
  "I'll come to call this home"
  ]

maxPhraseLength = Maybe.withDefault 0 <| List.maximum <| map String.length phrases

main =
  Html.program
    { init = init, view = view, update = update, subscriptions = subscriptions}

type alias Model = { currentTime : Time.Time, startTime : Time.Time}

init : (Model, Cmd Msg)
init =
  ({ currentTime = 0, startTime = 0}, Cmd.none)

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({
        currentTime = newTime,
        startTime = if model.startTime == 0 then newTime else model.startTime
      }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.second / 60) Tick

totalCycleTime = 40000
glitchClockTime = 75
phraseLengthTransition = 1000 / totalCycleTime

view : Model -> Html Msg
view model =
  (shuffle (List.range 0 15) 100) |> map (\index -> (toFloat index) + randFunc (11 * index + 32))|> map (oneofmany model) |> Html.div []

oneofmany : Model -> Float -> Html Msg
oneofmany model offset =
  let
    relativeTime = (-offset * totalCycleTime / 16) + (Time.inMilliseconds (model.currentTime - model.startTime))
    indexCont = relativeTime / totalCycleTime
    index = floor (indexCont)
    anim = indexCont - toFloat index
    glitchClock = floor <| relativeTime / glitchClockTime + (10 * offset)

    currentPhrase = if relativeTime < 0 then "" else if relativeTime < totalCycleTime && offset < 4 then prefix ++ phraseFirst else prefix ++ arrayGetCyclic (Array.fromList <| shuffle phrases (round (273*offset))) "" index

    totalPhrase = currentPhrase ++ (String.repeat (maxPhraseLength - String.length currentPhrase) " ")

    glitchBox = gsAll (giMode (if (randFunc glitchClock) > 0.1 then originalGlitch else boxGlitch)) totalPhrase
    glitchBit = gsAll (giBit
                        ((round (13 * (randFunc (glitchClock // 10))) - 2)
                        + (if (randFunc glitchClock) > 0.2 then 1 else 0)
                      )) totalPhrase

    glitchMask = List.range 1 (String.length totalPhrase) |> List.map (\index ->
      ((5 * (0.5 - randFunc(1000000*index + glitchClock)
       + 3 * (0.5 - randFunc(1000000*index + glitchClock // 4))))) * (1/(1 + 200 * anim)) - (max 0 <| -20 + 100 * (abs (anim - 1.0/4.0))) |> round)

    glitchPhrase = String.fromList (List.map4 (\original glitch1 glitch2 mask ->
                           if (abs mask) > 2 then ' ' else
                           if mask > 0 then glitch1 else
                           if mask < 0 then glitch2 else original
                           ) (String.toList totalPhrase) (String.toList glitchBox) (String.toList glitchBit) glitchMask)
  in
    Html.div [Html.Attributes.class "item"] [Html.div [Html.Attributes.class "inner"] (map (\letter -> Html.span [] [Html.text <| String.fromChar <|
      (if letter == ' ' then Char.fromCode(160) else letter)]) (String.toList <|glitchPhrase))]
