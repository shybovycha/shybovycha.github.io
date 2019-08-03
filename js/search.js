const getSearchIndexData = () => {
  const cachedSearchIndex = window.localStorage.getItem('search_index');

  if (cachedSearchIndex) {
    return Promise.resolve(JSON.parse(cachedSearchIndex));
  }

  return fetch('/word_list.json');
};

const cacheSearchIndexData = (index) => {
  window.localStorage.setItem('search_index', JSON.stringify(index));

  return index;
};

const buildIndex = ({ words, documents }) => {
  const index = new TrieMap();

  Object.entries(words)
    .forEach(([ word, occurrences ]) => index.add(word, occurrences));

  return index;
};

const search = (query, index) => {
  const queryWords = query.split(/\W+/);

  const tmpResults = queryWords.map(queryWord => index.find(queryWord))
    .reduce((acc, [ docId, occurrences ]) => { acc[docId] = (acc[docId] || 0) + 100; return acc; }, {});

  const results = new MaxHeap();

  Object.entries(tmpResults).forEach(([ docId, score ]) => results.add(docId, score));

  const sortedResults = [];

  while (results.length > 0) {
    sortedResults.push(results.remove().value);
  }

  return sortedResults;
};

const debounce = (element, eventName, timeout, eventHandler) => {
  element.addEventListener(eventName, (event) => {
    const now = new Date().getTime();

    if (!element.__debounce__) {
      element.__debounce__ = { [eventName]: { lastOccurrence: now, timeout: null } };
    } else if (!element.__debounce__[eventName]) {
      element.__debounce__[eventName] = {
        timeout: setTimeout(() => eventHandler.call(null, event), timeout),
        lastOccurrence: now
      };
    } else if ((now - element.__debounce__[eventName].lastOccurrence) <= timeout) {
      clearTimeout(element.__debounce__[eventName].timeout);
      element.__debounce__[eventName] = {
        timeout: setTimeout(() => eventHandler.call(null, event), timeout),
        lastOccurrence: now
      };
    }
  });
};

const addSearchHandler = (index) => {
  debounce(document.querySelector('#search'), 'keydown', 300, (event) => {
      const query = event.target.text;

      const results = search(query, index);

      console.log('Searching for:', query);
      console.log('Searc results:', results);
    });
};

document.addEventListener('DOMContentReady', () => {
  getSearchIndexData()
    .then(cacheSearchIndexData)
    .then(buildIndex)
    .then(addSearchHandler);
});
