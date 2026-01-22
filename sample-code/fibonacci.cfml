<!--- Fibonacci (CFML) --->
<cfset n = 10>
<cfset a = 0>
<cfset b = 1>

<cfoutput>
<h1>Fibonacci</h1>
<cfloop from="1" to="#n#" index="i">
  #a#<br>
  <cfset nextVal = a + b>
  <cfset a = b>
  <cfset b = nextVal>
</cfloop>
</cfoutput>

<!--- END OF FIBONACCI SAMPLE --->
<!--- END OF FIBONACCI SAMPLE --->
<!--- END OF FIBONACCI SAMPLE --->
