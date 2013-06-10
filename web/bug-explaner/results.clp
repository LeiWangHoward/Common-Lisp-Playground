<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" type="text/css" href="stylesheet.css">
</head>
<body>
<h1>Voting Results</h1>

<table border="1" summary="all favorites">
  <tr><th>Name</th><th>Favorite Food</th><th>Time of vote</th></tr>

  <clp_foreach var="voter" items="(vote:get-voters)">
    <tr>
      <th><clp_eval value="$voter" /></th>
      <td><clp_eval value="(vote:get-vote $voter)" /></td>
      <td><clp_eval value="(time:get-timestamp-string (vote:get-vote-time $voter))"/></td>
    </tr>
  </clp_foreach>
</table>

<p><a href="show/choice">Voting page</a></p>

<p><a href="show/login">Login page</a></p>

</body>
</html>
