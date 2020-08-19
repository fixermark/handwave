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
(define *current-note-bindings* (make-hash))  ; binding from note ID to note name
(define *initial-note-bindings* (make-gvector))
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
	     (let ([sharp-name (string->symbol (format "~a#~a" (car remaining-notes) cur-octave))]
		   [flat-name (string->symbol (format "~ab~a" (cadr remaining-notes) cur-octave))])
	       (hash-set! *note-name->bytes* sharp-name period)
	       (hash-set! *note-name->bytes* flat-name period))
	     (hash-set! *note-name->bytes* (string->symbol (format "~a~a" (car remaining-notes) cur-octave)) period))
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
  (let ([f (open-input-file fname)])
    (port-count-lines! f)
    (let ([top-level-form (read-syntax fname f)])
      (process-song top-level-form))
    (close-input-port f)))

(define (process-song song-form)
  (let ([inner (syntax-e song-form)])
    (cond [(< (length inner) 2) (error 'process-song "list ~a must have at least 2 elements" inner)])
    (let ([song-sym (syntax-e (car inner))]
	  [initial-assignments (syntax->list (cadr inner))]
	  [commands (cddr inner)])
      (cond [(not (eq? song-sym 'song)) (error 'process-song "list starting with ~a is not a song" song-sym)]
	    [(not initial-assignments) (error 'process-song "initial-assignments list ~a is not a list" (cadr inner))]
	    [(not (eq? (length initial-assignments) 16)) (error 'process-song "initial-assignments list ~a must have length 16, is length ~a"
							       (cadr inner)
							       (length initial-assignments))])
      (process-initial-assignments initial-assignments)
      (process-commands commands))))

;; stores initial assignments to current and initial note bindings
;; initial-assignments is a list of syntax objects of initial note names
(define (process-initial-assignments initial-assignments)
  (let ([i 0])
    (for ([assignment initial-assignments])
	 (let ([note (syntax-e assignment)])
	   (cond [(not (hash-has-key? *note-name->bytes* note)) (error 'process-initial-assignments "Note ~a does not exist" assignment)])
	   (hash-set! *current-note-bindings* i note)
	   (gvector-add! *initial-note-bindings* note)
	   (set! i (add1 i))))))

;; process the commands in a song
;; commands are syntax object list
(define (process-commands commands)
  (gvector-add! *output* "(bytes")
  (for ([command-syntax commands])
       (let ([command (syntax-e command-syntax)])
	 (cond
	  [else  ; individual note
	   (cond
	    [(integer? command)
	     (when (or (< command 0) (> command 15))
		   (error 'process-commands "note index ~a out of range" command-syntax))
	     (emit-single-note command)]
	     [else
	   (let ([note-id (note-name-to-id command)])
	     (when (not note-id)
		   (error 'process-commands "not a note name: ~a" command-syntax))
	     (emit-single-note note-id))])])))
  (gvector-add! *output* "#b00000001")
  (gvector-add! *output* ")"))

(define (emit-single-note note-id)
  (gvector-add! *output* (format "#b1~a010" (exact-to-four-bits note-id))))

(define (exact-to-four-bits exact)
  (let ([eights (if (eq? 0 (bitwise-and 8 exact)) "0" "1")]
	[fours (if (eq? 0 (bitwise-and 4 exact)) "0" "1")]
	[twos (if (eq? 0 (bitwise-and 2 exact)) "0" "1")]
	[ones (if (eq? 0 (bitwise-and 1 exact)) "0" "1")])
  (format "~a~a~a~a" eights fours twos ones)))

;; returns the ID of the note with the specified name, or #f if no handwave ID is holding that note
(define (note-name-to-id note-name)
  (let ([result (for/last ([i 17])
			  #:final (eq? note-name (hash-ref *current-note-bindings* i #f))
			  i)])
    (if (eq? result 17) #f result)))

(define (note-name-to-freq note-name)
  (cond [(not (hash-has-key? *note-name->bytes* note-name)) (error 'note-name-to-freq "~a is not a note" note-name)]
	[else (hash-ref *note-name->bytes* note-name)]))

(define (note-id-to-freq note-id)
  (cond [(or (< note-id 0) (> note-id 15)) (error 'note-id-to-freq "Note id ~a must be between 0 and 15" note-id)]
	[else (note-name-to-freq (hash-ref *current-note-bindings* note-id))]))

(define (byte->hi-low-string byte)
  (format "#x~x #x~x" (bitwise-and #xff (arithmetic-shift byte -8)) (bitwise-and #xff byte)))

(define (emit-output fname)
  (let ([f (open-output-file fname #:exists 'replace)])
    (for [(line *output*)]
	 (write-string line f)
	 (newline f))
    (close-output-port f)))

(define (emit-notes fname)
  (let ([f (open-output-file fname #:exists 'replace)])
    (write-string "(bytes\n" f)
    (for [(elem *initial-note-bindings*)]
	 (write-string (byte->hi-low-string (note-name-to-freq elem)) f)
	 (newline f))
    (write-string ")" f)
    (close-output-port f)))


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


