#lang racket

(require errortrace)
(require racket/cmdline)
(require racket/hash)
(require racket/math)
(require data/gvector)

;;;; Parser to convert songs into co2 binary representations, suitable for (include)
;;;; usage: racket song.scm -n song.not -o song.dat song.sng

(define *errors* (make-gvector))
(define *output* (make-gvector))
(define *current-note-bindings* (make-hash))
(define *initial-note-bindings* (make-hash))
(define *note-name->bytes* (make-hash))

(define *note-names* '("c" "d" "e" "f" "g" "a" "b"))
(define *sharpable-notes* '("c" "d" "f" "g" "a"))

;;; build *note-name->bytes* database
;;; code restructured from https://wiki.nesdev.com/w/index.php/APU_period_table

;; build semitone-period list as 16-bit integers from lowest piano A1 through G#7
;; "55.0" is the frequency of A1
(define (make-semitone-period-list)
  (let* ([ntscOctaveBase (/ 39375000.0 (* 22.0 16.0 55.0))]
	 [semitone (expt 2.0 (/ 12.0))]
	 [frequencies (for/list ([i 84])
				(* (arithmetic-shift 1 (quotient i 12)) (expt semitone (modulo i 12))))]
	 [periods (map (lambda (x) (- (exact-round (/ ntscOctaveBase x)) 1)) frequencies)]
	 [remaining-notes (list-tail *note-names* 5)]
	 [cur-octave 1]
	 [sharping #f])
    (for ([period periods])
	 (if sharping
	     (let ([sharp-name (format "~a#~a" (car remaining-notes) cur-octave)]
		   [flat-name (format "~ab~a" (cadr remaining-notes) cur-octave)])
	       (hash-set! *note-name->bytes* sharp-name period)
	       (hash-set! *note-name->bytes* flat-name period))
	     (hash-set! *note-name->bytes* (format "~a~a" (car remaining-notes) cur-octave) period))
	 (if (and (not sharping) (member (car remaining-notes) *sharpable-notes*))
	     (set! sharping #t)
	     (begin
		 (set! sharping #f)
		 (if (null? (cdr remaining-notes))
			    (begin
				(set! cur-octave (add1 cur-octave))
				(set! remaining-notes *note-names*))
			    (set! remaining-notes (cdr remaining-notes))))))))


(define (compile-sng fname out-fname notes-fname)
  (parse-file fname)
  (when (> (gvector-count *errors*) 0)
	(for [(e *errors*)]
	     (printf "~a\n" e))
	(exit 1))
  (emit-output out-fname)
  (emit-notes notes-fname))

(define (parse-file fname)
  (printf "Parsing happens here\n")
  (make-semitone-period-list)
  (for ([key (hash-keys *note-name->bytes*)])
       (gvector-add! *output* (format "~a: #x~x" key (hash-ref *note-name->bytes* key)))))


(define (emit-output fname)
  (let ([f (open-output-file fname #:exists 'replace)])
    (for [(line *output*)]
	 (write-string line f)
	 (newline f))
    (close-output-port f)))

(define (emit-notes fname) #f)

;;;; main
(define set-out (make-parameter #f))
(define set-notes (make-parameter #f))

(let* ((in-file (command-line
		 #:program "song"
		 #:once-each
		 [("-o" "--output") out "output song binary filaname" (set-out out)]
		 [("-n" "--notes") notes "output song notes filename" (set-notes notes)]
		 #:args (input) input))
       (out-file (set-out))
       (notes-file (set-notes)))
  (when (not out-file)
	(error "Expected -o <output>"))
  (printf "song: compiling ~a => ~a, ~a\n" in-file out-file notes-file)
  (compile-sng in-file out-file notes-file)
  (printf "song: done\n"))


