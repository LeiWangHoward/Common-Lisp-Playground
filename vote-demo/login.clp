<!DOCTYPE html>
<html>
<head>
<title>Login</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
</head>
<body>
<h1>Login Please</h1>

<wa_showerrors name="error" session clear />

<form action="do/signin" method="POST">
  <input type="hidden" name="after-login" value="<clp_value name="after-login" request/>">
  <div class="row">
  	<label for="name">Name</label>
  	<input id="name" name="name" type="text" value="<clp_value name="name" session />">
  </div>
   <div class="row">
  	<label for="password">Password</label>
  	<input id="password" name="password" type="password">
  </div>
  <div class="row">
  <input type="submit" value="login">
  </div>
</form>

</body>
</html>
