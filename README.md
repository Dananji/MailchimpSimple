# MailchimpSimple
A library to handle mailing lists in Mailchimp (http://mailchimp.com/)

# Features
This library is written in Haskell, to interact with Mailchimp's JSON API. The initial commit supported only the Mailchimp 2.0 version. Now the library is available for version 3.0 using Basic HTTP Authentication.
This implements the following functionalities;

- List the mailing lists for given 'apikey'
- List out the subscribers in a mailing list
- List the summary of activities of a given list
- Add individual subscribers and in batches
- Remove individual subscribers from a given list
- List the templates and campaigns in an account
- Create a new campaign
- Send a campaign (for this the campaign should be properly formatted using the web interface)
- Search members matching a specific query in a given list or account

Here are the links for the blog posts written on this [http://dananjiliyanage.blogspot.com/2015/07/develop-mailchimp-library-with-haskell.html, http://dananjiliyanage.blogspot.com/2016/03/using-basic-http-authentication-in.html]

# How to use

Given that you have installed Haskell platform in your machine; 
(If not, follow this link: https://www.haskell.org/platform/windows.html)

1. Clone the Git repository using 'git clone https://github.com/Dananji/MailchimpSimple.git'
2. Open the terminal, and browse into the cloned project folder
4. Run 'cabal update' 
3. Then type 'cabal install' in the terminal