req_ch <- alloc_chan [];
resp_ch <- alloc_chan [];

(* the emitter *)
spawn ([] => 
	loop <- (cnt =>
    sync (
			(cnt % 7 = 0) |> (
        (true => send req_chan) |
        (false => send resp_chan)
      )
    );
    loop (cnt + 1) 
  );
  loop 0
);

(* the accepter *)
spawn (loop <- ([] =>
  sync (
    chse [
      recv req_chan,
      recv resp_chan
    ]
  );
  loop ()
));

weak_until <- (p1, p2) # (
  recvd p2 \/ 
  recvd p1 /\ weak_until (p1, p2)
);

always <- p # (
  recvd p /\ always p 
);

solve {} # (
  always ([ch, _] #
    ch = resp_ch --> 
		weak_until (
      ([ch, _] # ~(ch = resp_ch)),
      ([ch, _] # ch = req_ch)
    )
  )
)
