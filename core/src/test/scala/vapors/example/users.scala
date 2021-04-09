package com.rallyhealth

package vapors.example

// Some sample users to compare for testing that generate the same facts every time
// this should hopefully make it easier to spot when something is awry with the sample user.
// Unlike JoeSchmoe, you cannot make any assumptions about the presence of facts so you should
// verify that your test user has the necessary pre-conditions. If not, you should pick a
// different generated user.

case object User1 extends GeneratedUser
case object User2 extends GeneratedUser
case object User3 extends GeneratedUser
