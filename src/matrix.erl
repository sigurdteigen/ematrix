%%%-------------------------------------------------------------------
%%% @author Sigurd Teigen <sigurd.teigen@gmail.com>
%%% @copyright (C) 2010, Sigurd Teigen
%%% @doc A library of matrix functions
%%%-------------------------------------------------------------------
-module(matrix).

%% API
-export([add/2,
	 all/2,
	 column/2,
	 diagonal/1,
	 dot/2,
	 fold/3,
	 from_list/1,
	 generate/3,
	 get/3,
	 identity/1,
	 is_equal/2,
	 is_square/1,
	 map/2,
	 mult/2,
	 new/2,
	 to_list/1,
	 row/2,
	 size/1,
	 trace/1,
	 transpose/1,
	 zipwith/3]).

%%%===================================================================
%%% API
%%%===================================================================

-record(matrix, {numrows :: pos_integer(),
		 numcols :: pos_integer(),
		 data :: array()}).

%% @doc Ordinary matrix addition (scalar).
-spec add(number(), #matrix{}) -> #matrix{};
	 (#matrix{}, #matrix{}) -> #matrix{}.		  
add(Scalar, Matrix) when is_number(Scalar) ->
    map(fun(_, _, Value) ->
		Scalar + Value
	end, Matrix);
%% @doc Ordinary matrix addition.
add(Matrix1, Matrix2) ->
    zipwith(fun(_, _, Term1, Term2) -> 
		    Term1 + Term2
	    end, Matrix1, Matrix2).

%% @doc True if the value of all cells pass the predicate
-spec all(fun((pos_integer(), pos_integer(), number()) -> boolean()),
	     #matrix{}) -> boolean().
all(Pred, Matrix) ->
    fold(fun(Row, Col, Value, Acc) ->
		 Acc and Pred(Row, Col, Value)
	 end, true, Matrix).

%% @doc The vector representing the column at the given index.
-spec column(pos_integer(), #matrix{}) -> [number()].
column(RowIndex, #matrix{numrows = NumRows, numcols = NumCols, data = Data}) ->
    lists:map(fun(Index) ->
		      array:get(Index, Data)
	      end, lists:seq(RowIndex, (NumRows * NumCols) - 1, NumCols)).

%% @doc Each element at (i,j) such that i = j.
-spec diagonal(#matrix{}) -> [number()].
diagonal(#matrix{numrows = NumRows, numcols = NumCols} = Matrix) ->
    lists:map(fun(Index) ->
		      get(Index, Index, Matrix)
	      end, lists:seq(0, erlang:min(NumRows, NumCols) - 1)).

%% @doc Ordinary vector dot product.
-spec dot(list(), list()) -> number();
	 (#matrix{}, #matrix{}) -> number().
dot(Vector1, Vector2) when is_list(Vector1) and is_list(Vector2) ->
    lists:sum(lists:zipwith(fun(Value1, Value2) ->
				    Value1 * Value2
			    end, Vector1, Vector2));
dot(#matrix{numrows = 1} = Vector1, #matrix{numrows = 1} = Vector2) ->
    zipwith(fun(_,_, Value1, Value2) ->
		    Value1, Value2
	    end, Vector1, Vector2).

%% @doc A value constructed by reducing the values of the matrix
%%      using the supplied function.
-spec fold(fun((non_neg_integer(), non_neg_integer(),
		number(), term()) -> term()),
	      term(), #matrix{}) -> term().
fold(Fun, InitialAcc, #matrix{numcols = NumCols, data = Data}) ->
    array:foldl(fun(Index, Value, Acc) ->
			Fun(Index div NumCols,
			    Index div NumCols,
			    Value, Acc)
		end, InitialAcc, Data).

%% @doc A matrix generated from a list of lists of numbers.
-spec from_list([[number()]]) -> #matrix{}.
from_list(ListOfLists) when length(ListOfLists) > 0 ->
    NumRows = length(ListOfLists),
    NumColsInFirstRow = length(hd(ListOfLists)),
    {true, _} = {lists:all(fun(Row) ->
				   length(Row) == NumColsInFirstRow
			   end, ListOfLists),
		 "Matrix must not be jagged"},
    #matrix{numrows = NumRows,
	    numcols = NumColsInFirstRow,
	    data = array:from_list(lists:flatten(ListOfLists))}.

%% @doc Generates a matrix given a function taking the current position
-spec generate(pos_integer(), pos_integer(),
	       fun((pos_integer(), pos_integer()) -> number())) -> #matrix{}.
generate(NumRows, NumCols, Fun) ->
    map(fun(Row, Col, _) ->
		Fun(Row, Col)
	end, new(NumRows, NumCols)).

%% @doc Gets the contents of the cell at the given position
-spec get(pos_integer(),pos_integer(),#matrix{}) -> any().
get(Row, Col, #matrix{numcols = NumCols, data = Data}) ->
    Index = (Row * NumCols) + Col,
    array:get(Index, Data).

%% @doc A matrix with 1's on the diagonal, 0's elsewhere.
-spec identity(pos_integer()) -> #matrix{}.
identity(Order) ->
    generate(Order, Order, fun(Row, Col) ->
				   case Row of
				       Col -> 1;
				       _ -> 0
				   end
			   end).

%% @doc True if for Matrix1(i,j) == Matrix2(i,j) for all (i,j)
is_equal(Matrix1, Matrix2) ->
    true = matrix:size(Matrix1) == matrix:size(Matrix2).

%% @doc True if the matrix is square, false otherwise.
-spec is_square(#matrix{}) -> boolean().
is_square(#matrix{numrows = NumRows, numcols = NumCols})
  when NumRows == NumCols ->
    true;
is_square(_) ->
    false.

%% @doc An matrix with all cells initialized to 0.
-spec new(pos_integer(), pos_integer()) -> #matrix{}.
new(NumRows, NumCols) ->
    #matrix{numrows = NumRows,
	    numcols = NumCols,
	    data = array:new([{size, NumRows * NumCols},
			      {default, 0},
			      {fixed, true}])}.

%% @doc The number of rows and columns of the matrix
-spec size(#matrix{}) -> {pos_integer(),pos_integer()}.
size(#matrix{numrows = NumRows, numcols = NumCols}) ->
    {NumRows, NumCols}.

%% @doc A matrix created from a mapping function.
-spec map(fun((pos_integer(), pos_integer(), number()) -> number()),
	     #matrix{}) -> #matrix{}.
map(Fun, #matrix{numcols = NumCols, numrows = NumRows, data = Data}) ->
    #matrix{numrows = NumRows,
	    numcols = NumCols,
	    data = array:map(fun(Index, Value) ->
				     Fun(Index div NumCols,
					 Index rem NumCols,
					 Value)
			     end, Data)}.

%% @doc Ordinary matrix multiplication (scalar).
-spec mult(number(), #matrix{}) -> #matrix{};
	  (#matrix{}, #matrix{}) -> #matrix{}.		  
mult(Scalar, Matrix) when is_number(Scalar) ->
    map(fun(_, _, Value) ->
		Scalar * Value
	end, Matrix);
%% @doc Ordinary matrix multiplication.
mult(#matrix{numcols = NumCols} = Matrix1, #matrix{numrows = NumRows} = Matrix2) ->
    NumCols = NumRows,
    generate(NumRows, NumCols, fun(Row, Col) -> 
				       dot(row(Row, Matrix1), column(Col, Matrix2)) 
			       end).

%% @doc The vector representing the row at the given index.
-spec row(pos_integer(), #matrix{}) -> [number()].
row(RowIndex, #matrix{numcols = NumCols, data = Data}) ->
    Start = RowIndex * NumCols,
    lists:map(fun(Index) ->
		      array:get(Index, Data)
	      end, lists:seq(Start, Start + NumCols - 1, 1)).

%% @doc Serializes a matrix to a list of lists (rows).
-spec to_list(#matrix{}) -> [[any()]].
to_list(#matrix{numrows = NumRows, numcols = NumCols} = Matrix) ->
    lists:map(fun(Row) ->
		      lists:map(fun(Col) ->
					get(Row, Col, Matrix)
				end, lists:seq(0, NumCols - 1))
	      end, lists:seq(0, NumRows -1)).

%% @doc The sum of the diagonal of the matrix
-spec trace(#matrix{}) -> number().
trace(Matrix) ->
    lists:sum(diagonal(Matrix)).

%% @doc A matrix such that the value at (i,j) is the value at (j,i)
%%      in the given matrix
-spec transpose(#matrix{}) -> #matrix{}.
transpose(Matrix) ->
    map(fun(Row, Col, _) ->
		get(Col, Row, Matrix)
	end, Matrix).

%% @doc Combines two matrices given a function
-spec zipwith(fun((pos_integer(), pos_integer(), number(), number()) -> number()),
		 #matrix{},#matrix{}) -> #matrix{}.
zipwith(Combine, Matrix1, Matrix2) ->
    true = matrix:size(Matrix1) == matrix:size(Matrix2),
    map(fun(Row, Col, Value1) ->
		Combine(Row, Col, Value1,
			get(Row, Col, Matrix2))
	end, Matrix1).
