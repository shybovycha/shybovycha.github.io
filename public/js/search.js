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

const debounce = (func, timeout) => {
  let timeoutId;

  return (...args) => {
    clearTimeout(timeoutId);

    timeoutId = setTimeout(() => {
      func.apply(null, args);
    }, timeout);

    if (!timeoutId) func.apply(null, args);
  };
};

const addSearchHandler = (index) => {
  document.querySelector('#search').addEventListener('keydown', 300, debounce((event) => {
      const query = event.target.text;

      const results = search(query, index);

      console.log('Searching for:', query);
      console.log('Searc results:', results);
    }));
};

document.addEventListener('DOMContentReady', () => {
  getSearchIndexData()
    .then(cacheSearchIndexData)
    .then(buildIndex)
    .then(addSearchHandler);
});
