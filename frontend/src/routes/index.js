
import Feed from '../components/Feed'

// Build a route with an aynchronously loaded component
const asyncRoute = ({path, componentPath, childRoutes}) => (
  (store) => ({
    path,
    childRoutes,
    getComponent (nextState, cb) {
      /*  Webpack - use 'require.ensure' to create a split point
          and embed an async module loader (jsonp) when bundling   */
      require.ensure([], (require) => {
        /*  Webpack - use require callback to define
            dependencies for bundling   */
        const component = require(componentPath).default(store)

        /*  Return getComponent   */
        cb(null, component)
      })
    }
  })
)

/*  Note: Instead of using JSX, we recommend using react-router
    PlainRoute objects to build route definitions.   */

const argumentsAPI = asyncRoute({
  path: 'arguments/:argumentid',
  componentPath: '../components/Argument'
})

const claimsAPI = (store) => ({
  path: 'claims',
  childRoutes: [

    asyncRoute({
      path: 'create',
      componentPath: '../components/CreateClaim'
    }),

    asyncRoute({
      path: ':claimid',
      componentPath: '../components/Claim',
      childRoutes: [

        asyncRoute({
          path: 'for',
          componentPath: '../components/CreateArgument',
        }),

        asyncRoute({
          path: 'against',
          componentPath: '../components/CreateArgument',
        }),
      ]
    })
  ]
})

const usersAPI = (store) => ({
  path: 'users',
  childRoutes: [

    asyncRoute({
      path: 'login',
      componentPath: '../components/Login'
    }),

    asyncRoute({
      path: 'logout',
      componentPath: '../components/Logout'
    }),

    asyncRoute({
      path: ':userid',
      componentPath: '../components/User'
    })
  ]
})

export default (store) => ({
  path: '/',
  component: <Feed store={store} />,
  childRoutes: [
    argumentsAPI,
    claimsAPI,
    usersAPI
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

