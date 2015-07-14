# MailchimpSimple
A library to handle mailing lists in Mailchimp (http://mailchimp.com/)

# Introduction
This library is written in Haskell, to interact with Mailchimp's JSON API. The initial commit supports only the Mailchimp 2.0 version.
This currently implements couple of methods in the 'lists' section, they are;
	- Adding subscribers
	- List out the subscribers in a mailing list
	- List the mailing lists for given 'apikey'
	- Past activities performed on a mailing list

I hope to develop this further to support Mailchimp version 3.0 as well. Here is the link for the blog post written on this [http://dananjiliyanage.blogspot.com/2015/07/develop-mailchimp-library-with-haskell.html]

# How to use

Given that you have installed Haskell platform in your machine; 
(If not, follow this link: https://www.haskell.org/platform/windows.html)

1. Clone the Git repository using 'git clone https://github.com/Dananji/MailchimpSimple.git'
2. Open the terminal, and browse into the cloned project folder
4. Run 'cabal update' 
3. Then type 'cabal install' in the terminal