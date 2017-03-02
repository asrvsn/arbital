import request from 'request'

// We only need to import the modules necessary for initial render
import CoreLayout from '../layouts/CoreLayout'
import Home from './Home'

const mirroredRoute = ({path, propKey, componentPath, childRoutes}) => (
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
            const props = {}
            props[propKey] = JSON.parse(response.body)
            cb(null, <Component {...props} />)
          } else {
            cb(null, <RedBox text={`failed to reach ${url}`} />)
          }
        })
        .

    /* Webpack named bundle   */
    }, 'counter')
  }
  })
)

/*  Note: Instead of using JSX, we recommend using react-router
    PlainRoute objects to build route definitions.   */

export const mirroredRoute({
  path: '/',
  propKey: 'items',
  componentPath: '../components/Feed',
  childRoutes: [
    mirroredRoute({
      path: ''
    })
  ]
})

export const createRoutes = (store) => ({
  path        : '/',
  component   : CoreLayout,
  indexRoute  : {component: Feed},
  childRoutes : [

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

export default createRoutes
