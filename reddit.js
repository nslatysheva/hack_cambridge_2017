<!-- Include the Pusher JavaScript library -->
<script src="http://js.pusher.com/2.2/pusher.min.js"></script>

<script>
  // Open a Pusher connection to the Realtime Reddit API
  var pusher = new Pusher("cbf29c93-840e-421c-af69-856cfcba01b5");

  // Subscribe to the /r/AskReddit subreddit (lowercase)
  var subredditChannel = pusher.subscribe("askreddit");

  // Listen for new stories
  subredditChannel.bind("new-listing", function(listing) {
    // Output listing to the browser console
    console.log(listing);
  });
</script>
