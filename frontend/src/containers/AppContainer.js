import React, { Component, PropTypes } from 'react'
import { browserHistory, Router, Route} from 'react-router'
import { Provider } from 'react-redux'
import Feed from '../components/Feed'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

import Header from '../components/Header'

class AppContainer extends Component {
  static propTypes = {
    store  : PropTypes.object.isRequired
  }

  shouldComponentUpdate () {
    return false
  }

  render () {
    const { store } = this.props
    // const children = (
    //   <div>
    //     <Header />
    //     <Route path='/' component={<Feed store={store} />} />
    //   </div>
    // )

    return (
      <Provider store={store}>
        <MuiThemeProvider>
          <div style={{ height: '100%' }}>
            <Router history={browserHistory} >
              <Route path='/' component={Feed} />
            </Router>
          </div>
        </MuiThemeProvider>
      </Provider>
    )
  }
}


export default AppContainer
