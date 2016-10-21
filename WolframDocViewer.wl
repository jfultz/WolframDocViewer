(* ::Package:: *)

(* ::Text:: *)
(*Installation instructions:*)
(*1) Create a file parallel to this one called credentials.wl.  It should contain the following:*)
(*	{"username", "token xxxxx"}*)
(*where username is your Github username, and xxxxx is a personal access token you've created.*)
(*2) Open this package in a notebook front end.  Ensure you are signed into the Wolfram Cloud.*)
(*3) Evaluate the bootstrap cell below.*)
(*4) Click the copy button to get the Markdown.  Fill in the user/repo/branch fields.*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)
(*DeleteDirectory[CloudObject["dev/WDV"],DeleteContents->True];*)
(*CloudPut[<||>,CloudObject["dev/WDV/blob_index.wl"]];*)
(*CloudPut[<||>,CloudObject["dev/WDV/commit_index.wl"]];*)
(*CloudPut[Get["credentials.wl"], CloudObject["dev/WDV/credentials.wl"], Permissions->"Private"];*)
(*$BlobIndex = $CommitIndex = <| |>;*)
(*CloudExport[Import["WolframDocViewer.wl","Text"],"Text",CloudObject["dev/WDV/WolframDocViewer.wl"]];*)
(*CloudExport[Import["badge2.png","PNG"],"PNG",CloudObject["dev/WDV/badge2.png"], Permissions->"Public"];*)
(*CloudExport[Import["nbicon.png","PNG"],"PNG",CloudObject["dev/WDV/nbicon.png"]];*)
(*Scan[CloudDeploy[Get[#],CloudObject["dev/WDV/"<>#]]&,FileNames["*","Stylesheets"]];*)
(*CloudDeploy[*)
(*APIFunction[{"user"->"String","repo"->"String","branch"->"String"},*)
(*HTTPRedirect[(Get[CloudObject["dev/WDV/WolframDocViewer.wl"]];*)
(*CommitObj[#user,#repo,#branch][[1]]),*)
(*<|"StatusCode"->302|>]&*)
(*],CloudObject["dev/WDV/wdv_api"],Permissions->"Public"];*)
(*Button["Copy", CopyToClipboard["[![View notebooks]("<>CloudObject["dev/WDV/badge2.png"][[1]]<>")]("<>CloudObject["dev/WDV/wdv_api"][[1]]<>"?user=&repo=&branch=)"]]*)


(* ::Section:: *)
(*Github support*)


Clear[GithubGet];
GithubGet[req_String,keys___String]:=
Module[{hr,res},
	hr=HTTPRequest[
		If[StringMatchQ[req,"https://api.github.com/"~~__],
			req,
			"https://api.github.com/"<>req],
		<|"User"->$GithubUserName,
			"UserAgent"->"Wolfram Document Viewer",
			"Headers"->{"Authorization"->$GithubToken}|>
	];
	res=ImportString[URLRead[hr]["Body"],"JSON"];
	Fold[#2/.#1&,res,{keys}]
]


(* ::Section:: *)
(*WDV*)


$BlobIndexObj = CloudObject["dev/WDV/blob_index.wl"];
$CommitIndexObj = CloudObject["dev/WDV/commit_index.wl"];
$CredentialObj = CloudObject["dev/WDV/credentials.wl"];
$SupportedTypes = "nb"|"cdf";
$BlobIndex = CloudGet[$BlobIndexObj];
$CommitIndex = CloudGet[$CommitIndexObj];
$NBIcon = CloudImport[CloudObject["dev/WDV/nbicon.png"]];
{$GithubUserName, $GithubToken} = CloudGet[$CredentialObj];


CommitObj[user_String, repo_String, branch_String] :=
Module[{commitreq},
	commitreq = Association @ GithubGet["repos/" <> user <> "/" <> repo <> "/git/refs/heads/" <> branch, "object"];
	If[MissingQ[$CommitIndex[commitreq["sha"]]], BuildCommitObj[user, repo, commitreq]];
	$CommitIndex[commitreq["sha"]]
]


BuildCommitObj[user_String, repo_String, commitreq_Association] :=
Module[{treereq, documents},
	treereq = GithubGet[commitreq["url"], "tree", "url"];
	documents = Select[
		Association /@ GithubGet[treereq <> "?recursive=1", "tree"],
		StringMatchQ[ToLowerCase @ FileExtension @ #path, $SupportedTypes] && #type === "blob" &];
	Scan[BuildDocumentObj, documents];
	$CommitIndex[commitreq["sha"]] = If[Length[documents] === 1,
		$BlobIndex[documents[[1]]["sha"]],
		BuildCommitTOC[user, repo, commitreq["sha"], documents]];
	CloudPut[$CommitIndex, $CommitIndexObj];
	CloudPut[$BlobIndex, $BlobIndexObj];
]


BuildCommitTOC[user_String, repo_String, sha_String, documents_List] :=
Module[{header1, header2, cell, obj},
	obj = CloudObject["dev/WDV/commits/" <> sha];
	header1 = ToBoxes[TextCell[
		Row[{"Github repo: ",
			Hyperlink[user, "https://github.com/"<>user], "/",
			Hyperlink[repo, "https://github.com/"<>user<>"/"<>repo],
			"\n",
			"SHA: ", Hyperlink[StringTake[sha, 10], "https://github.com/"<>user<>"/"<>repo<>"/commit/"<>sha]
		}], "Chapter"]][[1]];
	header2 = ToBoxes[TextCell["AVAILABLE NOTEBOOKS IN REPO:", "Text", Bold, CellFrame->{{0,0},{0,1}}]][[1]];
	cell = ToBoxes[
		ExpressionCell[Column[
			Hyperlink[
				Row[{$NBIcon, RawBoxes@"   ", TextCell[#path, FontFamily->"Helvetica", FontSize->16]}],
				$BlobIndex[#sha][[1]], BaseStyle->{FontColor->Black}]& /@ documents
			],
			"Text"],
		StandardForm][[1]];
	CloudDeploy[
		Notebook[
			{header1, header2, cell},
			Deployed->True,
			StyleDefinitions->Notebook[{
				Cell[StyleData[StyleDefinitions -> "Default.nb"]],
				Cell[StyleData["Hyperlink"], FontColor->RGBColor[0.866, 0.066, 0]],
				Cell[StyleData["Text"], CellMargins->{{27, 10}, {7, 7}}, CellFrameMargins->{{0, Inherited}, {Inherited, Inherited}}]
			}]
		], obj, Permissions->"Public"];
	obj
]


(* ::Text:: *)
(*This code is provisional to work around current limitations...*)


BuildDocumentObj[document_Association] :=
	If[MissingQ[$BlobIndex[document["sha"]]],
		$BlobIndex[document["sha"]] =
			CloudDeploy[StylifyNotebook @ DebuggifyNotebook @
				ImportString[GithubGet[document["url"],"content"], {"Base64"}],
					CloudObject["dev/WDV/blobs/" <> document["sha"]],
				Permissions->"Public"]
	];
DebuggifyNotebook[nbexpr_] := nbexpr /. Cell[CellGroupData[{Cell[_, "SlideShowNavigationBar", ___], cells__Cell}, _]] :> cells
StylifyNotebook[nbexpr_] :=
Module[{stylesheet = StyleDefinitions /. Options[nbexpr]},
	If[MatchQ[stylesheet, FrontEnd`FileName[{"Wolfram"}, ("Function"|"Message"|"Guide"|"Tutorial")~~"PageStyles.nb", _]],
		nbexpr /. (StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, file_, _]) :>
			(StyleDefinitions->CloudGet[CloudObject["dev/WDV/Wolfram/"<>file]]),
		nbexpr
	]
]


(* ::Text:: *)
(*Instead, we want something more like the following (but we need to be able to set MIME types):*)


(*
BuildDocumentObjs[document_Assocation] :=
	If[MissingQ[$BlobIndex[document["sha"]]],
		$BlobIndex[document["sha"]] = CloudExport[
			ImportString[GithubGet[document["url"], "content"], {"Base64", "Byte"}],
			"Byte",
			CloudObject["dev/WDV/blobs/"<>document["sha"] <> "." <> FileExtension[document["path"]]]]
	]
*)


(* ::Subsubsection::Closed:: *)
(*Examples*)


(* ::Input:: *)
(*CommitObj["WolframResearch","GitLink-Talk","master"]*)


(* ::Input:: *)
(*CommitObj["jfultz","NotebookTest","master"]*)
