
var app = new PusherPlatform.App({
  appId: 'cbf29c93-840e-421c-af69-856cfcba01b5',
});

var myFeed = app.feed('playground');

myFeed.subscribe({
  onOpen: () => console.log('Connection established'),
  onItem: item => console.log('Item:', item),
  onError: error => console.error('Error:', error),
});

myFeed.append('Hello, world!')
  .then(response => console.log('Success:', response))
  .catch(err => console.error('Error:', err));

// You’re not limited to appending string values;
// you can also append objects, arrays and numbers.
myFeed.append({ yourKey: 'your value' })
  .then(response => console.log('Success:', response))
  .catch(err => console.error('Error:', err));
