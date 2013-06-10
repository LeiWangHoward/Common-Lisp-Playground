<!DOCTYPE html>
<html>
<head>
<title>Choose favorite</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<script src="choice.js"></script>
</head>
<body>

<h1>Vote</h1>

<p>
  OK, <clp_value name="name" session/>, what do you like? Select an
  item already given in the past, or enter something new.
</p>


<clp_eval var="favorite" value="(vote:get-vote $name)"/>

<form method="post" action="do/vote">
  <div class="row">
  	<label for="food">Favorite</label>
  	<input id="food" name="food" type="text" value="<clp_value name="favorite" />">
    ... enter your favorite food
  </div>
  <clp_when test="(not (null (vote:get-choices)))">
    <div class="row">
    	<label for="sel_Choice">Choice</label>
    	<select id="sel_Choice" onchange="useSelection(this)">
        <option id="no-choice">Select...</option>
        <clp_foreach var="food" items="(vote:get-choices)">
          <option value="<clp_value name="food"/>">
             <clp_value name="food"/>
          </option>
        </clp_foreach>
     	</select>
      ....or select something others have picked
    </div>
  </clp_when>
  <div class="row">
  <input type="submit" value="Submit vote">
  </div>
</form>

<p>
  <clp_choose>
    <clp_when test="(null $favorite)">
      Nothing selected yet.
    </clp_when>
    <clp_otherwise>
      You picked <clp_value name="favorite" />
    </clp_otherwise>
  </clp_choose>
</p>

<p><a href="show/login">Login page</a></p>

</body>
</html>
