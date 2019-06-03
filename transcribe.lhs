
\usepackage{gnuplottex}

This project will automatically trascribe a recording of a snare drum solo.
Since this is a probject for a 10 week class, I'm making a lot of simplifying assumptions here.
First, I'm only transcribing the notes themselves.
I'm not worrying about dynamics or accents.
I'm also assuming that there is only one instrument.
Finally I'm assuming all pieces are in common time.
This is likely not true, but since I don't keep dynamic or accent informationi,
it'd be difficult, if not impossible, to guess at other time signatures.

With those caveats there is still a lot of work to do.
First we must read in the wave file.  I've be using the haskell WAVE library for this.
Then we need to determine what point in time each note was hit.
This is the beat detection algorithm.
Next we need to make a reasonable guess at the tempo of the song.
Finally given the time of each note, and the tempo, we need to determine where each note lands in the piece.
This is piece reconstruction.
Finally we write the notes out to a latex file in the lilypond format.

> main = do args <- getArgs 
>           file <- readFile (head args)
>           beats <- beatDetect file
>           tempo <- getTempo beats
>           notes <- pieceRecontruction beats tempo
>           writeFile tempo notes

\subsection*{Data Types}

Before we get started, let's make a few definitions.
There are 10 possible notes we are going to allow, divided into two categories.
Straight notes are whole, half, quarter, eighth, sixteenth, and $32^{nd}$ notes.
Tirplets are divided into half, quarter, eighth, and sixteenth note triplets.

> type Subdivision = Int
> data Note = Straight Subdivision Int
>           | Triplet Subdivision Int

While songs may contain tripelts, there is an underlying assumption that all triplets
begin on a beat.
For example, we'd like to avoid this nonsence.
\includegraphics{quarterTriple}

A Tempo is an integer within the range $\{60\ldots 300\}$
While we could have music at other tempos, it's not likely to come up.

> type Tempo = Int

A wave file is a very simple music file format
consisting of a list of samples.
A simple is just a measuremean of the amplitude of a wave between 0 and 1.
This representation of music is Pulse Coded Modulation,
which is the format I'll be working with for most of the algorithms.

> type PCM = [Double]

\subsection*{Beat Detection}

Our first task is to determine when a drum beat has occurred in our wave file.
There are several ways to do beat detection, but my approach is to use is to find the energy of the wave over a small window.
If the window has a high amount of energy, then that means that a beat probably happened within that window.
So, to determine when a beat happens, we find the energy over a sliding window, and return the points where the
energy jumps rapidly.

This has a few advantages over looking of any spikes in the wave.
The first is that snare drum hits are themselves waves, so they'll possibly have many spikes in a short amount of time.
We could solve this with a lowpass filter, but then we risk losing smaller snare drum hits.

Another option is to look at the second dirivative of the wave, and look for concave points.
However, this suffers from the same problem.
The second derivative of a wave is still a wave, and will still produce many spikes.
This approach at least has the advantage of amplifying smaller waves.
if our wave is $w(t) = A \sin(\omega t)$ then $\frac{dw}{dt} = -A\omega^2 \sin(\omega t)$.
So, as long as our frequence $\omega$ is larger than 1 rad/s, the wave will be amplified.

So, how do we compute the energy of the wave $w$.
It's really easy.  $E_{[a,b]} = \frac{\int_a^b w^2(t)}{b-a}$.
Since our window is always the same size we don't need to worry about the proportionallity constant.
This is a pretty standard integral, so we square the wave, and sum each windown along the wave.
We can do this in linear time with a sliding window.
This is the $getEnergy$ function.

Next, we filter out all beats that doesn't have a high enough energy.
Right now this is set at 70\% of the maximum energy, but that can be changed.

Finally we record what sample each beat was at.
We only take the first beat that survived our filter.
this avoids the problem of deciding if a large energy is a beat, or simply an earlier beat decaying.


> beatDetect :: PCM -> Beats
> beatDetect pcm = beats False . candidates 0.7 . energy 20 . map (^2)
>  where  getEnergy  _       []      _  =  []
>         getEnergy  (x:xs)  (y:ys)  e  =  e : getEnergy xs ys (e - x + y)
>         energy n xs = getEnergy xs (drop n xs) (sum $ take n xs)
>
>         candidates p e = filter (> p*maximum e) e
>
>         beats  _  _      []      =  []
>         beats  c  True   (0:cs)  =  beats (c+1) False cs
>         beats  c  True   (_:cs)  =  beats (c+1) True cs
>         beats  c  False  (0:cs)  =  beats (c+1) False cs
>         beats  c  False  (_:cs)  =  c : beats (c+1) True cs

\subsection*{Tempo}

Tempo reconstruction is easily the hardest part of this project.
There are a few different approaches to this problem.

The first approach is to use a histogram, and just pick the tempo that had
the most hits.
This is problematic for several reasons, but it's a reasonable place to start.

The next approach is to use a phase lock loop.
The idea is pretty straight forward.
We want to build a machine outputs a wave at the same frequency as the tempo of the music.
If the frewuencies of two waves are the same, then the phase of the two waves
will remain constant.
So, the We'd want to build a machine that locks the phase in.
Unfortunately this approach has one major drawback.
We don't have a wave.  We have discrete pulses.
So, while we could generate a click track at a constant frequence, and measure
the difference, we can't use that result to feed back into our click track.

The final common approach is to use autocorrelation.
Again the idea is pretty simple.
we compare our piece to a piece that is offset by a short time.
If that short time is the length of a beat, then we'd expect the offset track to sync up with the origonal track.
This seems promising, but I had my own idea that I wanted to try.

As far as I'm aware this idea is entirely novel.
That probably means that it's bad, but hey, I wanted to see.
It's inspired off the of video ``Music and Measure Theory'' \cite{measure}.
That video was aimed at determining if two pitches were in tune,
but the problem of finding out if two tempos match up
is really the same.  It's just a much slower version of that problem.

To do this I compute an ``error'' for each note that was played.
Then the tempo with the smallest error is probably the right one.
This does have the side effect of being heavily biased towards faster tempos.


> getTempo :: Beats -> Tempo
> getTempo beats = fst . minimumBy cmpErr . zip tempos . map (measureError beats) tempos
>  where tempos = [60..300]
>        cmpErr (t1,e2) (t2,e2) = e1 < e2

In order to measure the error for a note, I compare the nearest 32nd note, and the nearest 16th note triplet.

> measureError :: SampleRate -> Time -> Tempo -> Double
> measureError sampleRate beat bpm = min (error 32 note32) (error 24 triple16)
>  where spb       = (sampleRate*60) `div` bpm
>        note32    = spb `div` 8
>        triple16  = spb `div` 6

Inorder to actually measure the error, I develop a new distance from the note to the start of a measure.
However, the metric I need is a little weird.
I actually need to use a 2-adic metric space.
This idea also came from a youtube video \cite{padic}.
The idea is that the note at the start of the measure is the most important,
then the half note, then the quarter notes, and so on.
So we measure how close we are to the nearest 32nd note, and multiply by a scaling factor
by the importace of the note.
formally, let $\sigma$ the value of the nearest $32^{nd}$ note $p$.
$d(x,y)$ be the standard distance on $\R^1$.
Then the our new error function is $e(n) = 2\cdot\sigma |d(p,n)|$.
For example, if $p$ is the thrid quarter note, and we're a distance of 1/128 past the note, then our error is
$e(n) = 2\cdot 4\cdot |-1/128| = 1/16$

If we graph the error function for both 32nd notes and 16th note tripplets, then we get the following graph.
You can see that more prominent notes, like the half note, have a much lower error.

\begin{gnuplot}
set xlabel "time from start of measure (in 64th note triplets)"
set ylabel "error of note"
plot "note.dat" with lines, "triple.dat" with lines
\end{gnuplot}

In order to actually compute the error first I move our note a 64th note forwards,
so that I can take the previous 32nd note instead of finding the nearest one.
Then I get th

> error :: Time -> Int -> Int -> Double
> error note subdivision duration = fromIntegral (2 * sigma * dist position center) / fromIntegral subdivision
>   where halfDuration  = duration `div` 2
>         measure       = duration * subdivision
>         center        = (note + halfDuration) `mod` measure
>         position      = center `div` duration
>         sigma         = fromIntegral $ gcd subdivision position
>         dist p n      = abs (n - (2*p+1)*halfDuration)



cite measure https://www.youtube.com/watch?v=cyW5z-M2yzw
cite padic https://www.youtube.com/watch?v=XFDM1ip5HdU
