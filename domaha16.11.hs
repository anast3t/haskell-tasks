data Author = Author { --автор является корнем всех трэков, связанных с ним
    author_name::String,
    author_albums::[Album],
    author_tracks::[Track]
} deriving(Show, Eq)

data Quality = Opus|MP3|FLAC|MLP deriving (Show, Eq)
data Config = Mono|Stereo|Surroud5LFE|Surround7LFE deriving(Show, Eq)
data Track = Track { --по всем этим типам можно производить поиск, объединять в альбомы и радоваться жижзни 
    track_author::String, --Author
    track_title::String, 
    track_lengthInSec::Integer,
    track_quality::Quality,
    track_maxSoundConfig::Config
} deriving (Show, Eq)

data Album = Album {
    album_author::String, --Author 
    album_name::String, 
    album_length::Integer, 
    album_track_number::Integer, 
    album_tracks::[Track] --titles
} deriving(Show, Eq)

data Playlist = Playlist {
    pl_User::String, --User 
    pl_name::String, 
    pl_length::Integer, 
    pl_track_number::Integer, 
    pl_tracks::[Track]
} deriving(Show, Eq)

data IsFavourite = IsFavouriteAlbum Album Bool | IsFavouriteTrack Track Bool | IsFavouriteAuthor Author Bool deriving(Show, Eq)

data User = User {
    user_name::String,
    user_soundConfig::Config,
    user_albums::[Album],
    user_tracks::[Track],
    user_authors::[Author]
}deriving(Show, Eq)
---------------------------------------------------------------
authorError = Author "" [] []
--Authors examples
dimas = Author "Dimas" [dima_alb] [bass, kachnulo]
tolyan = Author "Tolyan" [] []
--Tracks examples
bass = Track "Dimas" "rascolbas" 120 Opus Mono 
kachnulo = Track "Dimas" "bochkabass" 420 MP3 Stereo
dolbitNormalno = Track "Tolyan" "kolbasitSolo" 228 MLP Surround7LFE
--Album examples
dima_alb = Album "Dimas" "Kolbaser" (sumLength dima_tracks) (trackNumber dima_tracks) dima_tracks
dima_tracks = [kachnulo]
--Bases
author_base = [dimas, tolyan]

name::Track -> String
name x = track_title x

sumLength::[Track]->Integer
sumLength [] = 0
sumLength ((Track author name leng qual conf):xs) = leng + sumLength xs

trackNumber::[Track]->Integer
trackNumber [] = 0
trackNumber (x:xs) = 1 + trackNumber xs
--все функции будут возвращать именно базу(чет как-то не получается оперировать с ней напрямую), тк через неё строятся все привязки(да, это неоптимально, но крупных исполнителей в мире не больше 1 000 000, а в обычном муз-сервисе не более 10000, а если еще и представить, что это персональная программа, то вообще кайф)
createAuthor::String -> [Album] -> [Track] -> [Author]
createAuthor name albums tracks = (Author name [] []):author_base

addTrackToBase::String->String->Integer->Quality->Config->[Author] --по факту эта функция делает всё и сразу, что нужно сделать при создании трека и возвращает обновленную базу(потому что, очень неожиданно, некуда сохранять созданный трек, и нужно как-то выкручиваться)
addTrackToBase author title leng qual conf = if findAuthor /= authorError
    then 
        if findTrack /= [] 
            then
                author_base
            else
                (Author author (author_albums findAuthor) (newTrack:(author_tracks findAuthor))):removed 
    else 
        createAuthor author [] (newTrack:[]) where 
    findAuthor = findAuthorF author
    removed = deleteAuthor author
    findTrack = filter(\x -> x == newTrack) (author_tracks findAuthor)
    newTrack = Track author title leng qual conf
--идентично и с 
createAlbum::String->String->[Track]->[Author] --дальше уже подразумеваю, что и автор существует, и трэки
createAlbum author name tracks = (Author author (newAlbum:(author_albums findAuthor)) (author_tracks findAuthor)):removed where 
    findAuthor = findAuthorF author
    removed = deleteAuthor author
    newAlbum = Album author name (sumLength tracks) (trackNumber tracks) tracks

addTrack2Album::Track->String->[Author]
addTrack2Album track album = (Author author (newAlbum:removedA) (author_tracks findAuthor)):removed where
    findAuthor = findAuthorF author
    removed = deleteAuthor author
    findAlbum = filter (\x -> (album_name x) == album) (author_albums findAuthor)
    firstAlbum = head findAlbum
    removedA = filter (\x -> (album_name x) /= album) (author_albums findAuthor)
    tracks = track:(album_tracks firstAlbum)
    newAlbum = Album author (album_name firstAlbum) (sumLength tracks) (trackNumber tracks) tracks
    author = (track_author track)
    
findAuthorF::String->Author
findAuthorF author= if findAuthor /= [] then firstA else authorError where --потом заменить на Maybe!!!!!!!!!!!!
    findAuthor = (filter (\x -> author_name x == author) author_base)
    firstA = head findAuthor
deleteAuthor::String->[Author]
deleteAuthor author = filter (\x -> author_name x /= author) author_base

{- findAlbumF::String->Album
findAlbumF name = if findAlbum /= [] then firstA else error"Create Author" where --потом заменить на Maybe!!!!!!!!!!!!
    findAlbum = filter (\x -> (album_name x) == album) (author_albums )
    firstA = head findAlbum -}


