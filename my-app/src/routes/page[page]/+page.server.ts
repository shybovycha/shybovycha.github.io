import type { PageLoad } from './$types';

export const load: PageLoad = ({ params }) => {
  return {
    page: parseInt(params.page.replace(/^.*(\d+).*$/, '$1')) - 1,
  };
};

