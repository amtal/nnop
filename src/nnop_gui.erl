-module(nnop).

-export([main/0]).

layout() ->
    %% This layout was greated using Eclipse
"
<LinearLayout xmlns:android=\"http://schemas.android.com/apk/res/android\"
    xmlns:tools=\"http://schemas.android.com/tools\"
    android:layout_width=\"match_parent\"
    android:layout_height=\"match_parent\"
    android:orientation=\"vertical\"
    android:background=\"#ffffff\" >

    <LinearLayout
        android:layout_width=\"match_parent\"
        android:layout_height=\"wrap_content\" >

        <TextView
            android:id=\"@+id/textView3\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:text=\"Server:\" />

        <EditText
            android:id=\"@+id/serverText\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:layout_weight=\"1\"
            android:ems=\"10\" />

    </LinearLayout>

    <LinearLayout
        android:layout_width=\"match_parent\"
        android:layout_height=\"wrap_content\" >

        <TextView
            android:id=\"@+id/textView1\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:text=\"Enter value:\" />

        <EditText
            android:id=\"@+id/inputText\"
            android:layout_width=\"0dip\"
            android:layout_height=\"wrap_content\"
            android:layout_weight=\"1\"
            android:ems=\"10\"
            android:inputType=\"number\" >

            <requestFocus />
        </EditText>

        <Button
            android:id=\"@+id/facButton\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:text=\"Fac!\" />

    </LinearLayout>

    <LinearLayout
        android:layout_width=\"match_parent\"
        android:layout_height=\"wrap_content\" >

        <TextView
            android:id=\"@+id/textView2\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:text=\"Result:\" />

        <TextView
            android:id=\"@+id/resultView\"
            android:layout_width=\"wrap_content\"
            android:layout_height=\"wrap_content\"
            android:textAppearance=\"?android:attr/textAppearanceLarge\" />

    </LinearLayout>

</LinearLayout>
".

main() ->
    nnop_port:run(2000,2010,["backdoor"],[]),
    hang().
%    android:fullShow(layout()),
%    eventLoop().

hang() ->
    _Event = android:eventWait(),
    hang().

eventLoop() ->
    Event = android:eventWait(),
    case Event of
	{struct, Props} ->
	    case proplists:get_value("name", Props) of
		"click" ->
		    {struct, Data} = proplists:get_value("data", Props),
		    ID = proplists:get_value("id", Data),
		    case ID of
			"facButton" ->
			    Res = lists:flatten(io_lib:format("~w",[test()])),
			    android:fullSetProperty("resultView", "text", Res);
			_ ->
			    ok
		    end;
		"key" ->
		    {struct, Data} = proplists:get_value("data", Props),
		    Key = proplists:get_value("key", Data),
		    case Key of
			"4" ->
			    %% key 4 is the back key; pressing this will
			    %% stop the application
			    init:stop();
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    eventLoop().
		
test() ->
    IP = get_property("serverText", "text"),
    {ok, Socket} = gen_tcp:connect(IP, 2000, [binary,{packet,4}]),
    case catch list_to_integer(get_property("inputText", "text")) of
	{'EXIT', _} ->
	    %% not an integer yet
	    0;
	Value ->
	    gen_tcp:send(Socket, term_to_binary({fac,Value})),
	    Result = receive
		      {tcp, Socket, Bin} ->
			  binary_to_term(Bin)
		  end,
	    gen_tcp:close(Socket),
	    Result
    end.

get_property(ID, Prop) ->
    case android:fullQueryDetail(ID) of
	{struct, P} ->
	    proplists:get_value(Prop, P);
	_ ->
	    undefined
    end.
