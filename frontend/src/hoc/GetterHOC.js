import React, { Component } from 'react'
import Redbox from 'redbox-react'
import { connect } from 'react-redux'

import LinearProgress from 'material-ui/LinearProgress';

import backend from '../util/backend'
import AuthenticatedHOC from './AuthenticatedHOC'

/**
  NOTE: if using this HOC, do not use AuthenticatedHOC!
  **/

const styles = {
  loader: {
    top: '50%',
    left: '50%',
    transform: 'translateY(-50%, -50%)'
  }
}

export default (ChildComponent, getter) => {
  class GetterComponent extends Component {
    constructor(props) {
      super(props)
      this.state = {
        childProps: {},
        gettingState: 'LOADING',
        error: ''
      }
      this.backendUrl = window.location.hostname + '5000'
    }

    componentDidMount() {
      const { session } = this.props

      getter(this.props).forEach(item => {
        const {path, mapResponseToProps} = item

        backend
          .authenticate(session.id)
          .get(path, (err, response, body) => {
            if (err !== null) {

              this.setState({gettingState: 'FAILED', error: err})

            } else {
              if (response.statusCode == 200) {

                const { childProps } = this.state
                const newChildProps = Object.assign(
                  {},
                  childProps,
                  mapResponseToProps(JSON.parse(body))
                )
                this.setState({childProps: newChildProps, gettingState: 'SUCCEEDED'})

              } else {

                this.setState({gettingState: 'FAILED', error: response.statusMessage})

              }
            }
          })
      })
    }

    render() {
      const { childProps, gettingState, error } = this.state

      switch(gettingState) {
        case 'LOADING':
          return (
            <div style={styles.loader}>
              <span>Fetching data</span>
              <LinearProgress mode="indeterminate" />
            </div>
          )
        case 'FAILED':
          return <h1>{"Error fetching data: " + error}</h1>
        case 'SUCCEEDED': {
          const allChildProps = Object.assign({},
            this.props,
            childProps
          )
          return <ChildComponent {...allChildProps} />
        }

      }
    }
  }

  return AuthenticatedHOC(GetterComponent)
}
