-module(heap).
-export([new/1,insert/2,extract_min/1]).

new(CompareFunc) -> {heap, CompareFunc, 0, none}.

insert(Element, {heap, CompareFunc, Count, CurrentTree}) ->
    {heap, CompareFunc, Count+1, add_element(Element, CompareFunc, CurrentTree)}.


add_element(Element, _, none) -> {e, Element, none, none};
add_element(Element, CompareFunc, {e, Val, none, RightTree}) ->
    ElementIsSmaller= CompareFunc(Element, Val),
    if
        ElementIsSmaller -> {e, Element, {e, Val, none, none}, RightTree};
        true -> {e, Val, {e, Element, none, none}, RightTree}
    end;
add_element(Element, CompareFunc, {e, Val, LeftTree, none}) ->
    ElementIsSmaller= CompareFunc(Element, Val),
    if
        ElementIsSmaller -> {e, Element, LeftTree, {e, Val, none, none}};
        true -> {e, Val, LeftTree, {e, Element, none, none}}
    end;
add_element(Element, CompareFunc, {e, Val, LeftTree={e, LeftChild,_,_}, RightTree={e, RightChild,_,_}}) ->
    LeftIsSmallerThanRight= CompareFunc(LeftChild, RightChild),
    ElementIsSmaller= CompareFunc(Element, Val),
    if
        ElementIsSmaller ->
            if
                LeftIsSmallerThanRight -> {e, Element, LeftTree, add_element(Val, CompareFunc, RightTree)};
                true -> {e, Element, add_element(Val, CompareFunc, LeftTree), RightTree}
            end;
        true ->
            if
                LeftIsSmallerThanRight -> {e, Val, LeftTree, add_element(Element, CompareFunc, RightTree)};
                true -> {e, Val, add_element(Element, CompareFunc, LeftTree), RightTree}
            end
    end.


extract_min({heap, CompareFunc, Count, CurrentTree={e, Val, _, _}}) ->
    {Val, {heap, CompareFunc, Count-1, remove_element(CompareFunc, CurrentTree)}}.


remove_element(_, {e, _, none, none}) -> none;
remove_element(CompareFunc, {e, _, LeftTree={e, LeftChild,_,_}, none}) -> {e, LeftChild, remove_element(CompareFunc, LeftTree), none};
remove_element(CompareFunc, {e, _, none, RightTree={e, RightChild,_,_}}) -> {e, RightChild, none, remove_element(CompareFunc, RightTree)};
remove_element(CompareFunc, {e, _, LeftTree={e, LeftChild,_,_}, RightTree={e, RightChild,_,_}}) ->
    LeftIsSmallerThanRight= CompareFunc(LeftChild, RightChild),
    if
        LeftIsSmallerThanRight -> {e, LeftChild, remove_element(CompareFunc, LeftTree), RightTree};
        true -> {e, RightChild, LeftTree, remove_element(CompareFunc, RightTree)}
    end.