import request from 'request'

// We only need to import the modules necessary for initial render
import CoreLayout from '../layouts/CoreLayout'

const mirroredRoute = ({path, mapResponseToProps, componentPath, childRoutes}) => (
  (store) => ({
    path,
    childRoutes,
    getComponent (nextState, cb) {
    /*  Webpack - use 'require.ensure' to create a split point
        and embed an async module loader (jsonp) when bundling   */
    require.ensure([], (require) => {
      /*  Webpack - use require callback to define
          dependencies for bundling   */
      const Component = require(componentPath).default

      /*  Return getComponent   */
      const url = nextState.location.path // TODO this could be wrong
      request
        .get(url)
        .on('response', (response) => {
          if (response.statusCode == 200) {
            const props = mapResponseToProps(response)
            cb(null, <Component {...props} />)
          } else {
            cb(null, <RedBox text={`failed to reach ${url}`} />)
          }
        })
        .

    /* Webpack named bundle   */
    }, componentPath)
  }
  })
)

/*  Note: Instead of using JSX, we recommend using react-router
    PlainRoute objects to build route definitions.   */

export default mirroredRoute({
  path: '/',
  mapResponseToProps: (resp) => {feed: JSON.parse(resp.body)},
  componentPath: '../components/Feed',
  childRoutes: [

    // mirrored routes
    mirroredRoute({
      path: 'arguments/:argumentid',
      componentPath: '../components/Argument',
      mapResponseToProps: (resp) => {argument: JSON.parse(resp.body)},
    }),

    mirroredRoute({
      path: 'claims/:claimid',
      componentPath: '../components/Claim',
      mapResponseToProps: (resp) => {claim: JSON.parse(resp.body)},
      childRoutes: [
        (store) => ({
          path: 'for',
          component: <CreateArgument for={true} claimId={/* TODO */} />,
        }),
        (store) => ({
          path: 'against',
          component: <CreateArgument against={true} claimId={/* TODO */} />,
        })
      ]
    }),

    mirroredRoute({
      path: 'feed',
      componentPath: '../components/Feed',
      mapResponseToProps: (resp) => {feed: JSON.parse(resp.body)},
    }),

    mirroredRoute({
      path: 'users/:userid',
      componentPath: '../components/User',
      mapResponseToProps: (resp) => {user: JSON.parse(resp.body)},
    }),

    // non-mirrored routes
    (store) => ({
      path: 'claims/create',
      component: <CreateClaim />,
    }),

    (store) => ({
      path: 'arguments/create',
      component: <CreateArgument />,
    }),

  ]
})

/*  Note: childRoutes can be chunked or otherwise loaded programmatically
    using getChildRoutes with the following signature:

    getChildRoutes (location, cb) {
      require.ensure([], (require) => {
        cb(null, [
          // Remove imports!
          require('./Counter').default(store)
        ])
      })
    }

    However, this is not necessary for code-splitting! It simply provides
    an API for async route definitions. Your code splitting should occur
    inside the route `getComponent` function, since it is only invoked
    when the route exists and matches.
*/

